{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Arrow                          (second, (&&&))
import qualified           Control.Monad.State as State
import           Criterion
import           Criterion.Types                        (Report (..),
                                                         SampleAnalysis (..))
import qualified Data.Binary                            as Bin
import qualified Data.ByteString.Lazy                   as B
import qualified Data.Colour.SRGB                       as Colour
import           Data.List                              (find)
import           Data.Maybe                             (maybeToList)
import           Data.Monoid                            ((<>))
import qualified Data.Text                              as T
import           GHC.Generics                           (Generic)
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy
import qualified Options.Generic                        as Opts
import           Statistics.Resampling.Bootstrap        (Estimate (..))

import qualified Freer
import qualified Mtl


data Bench a = Bench {
      benchDescription :: T.Text
    , benchData        :: [(Double, a)]
    }
    deriving (Functor, Generic, Show, Foldable, Traversable)

data BenchGroup a = BenchGroup {
      bgDescription :: T.Text
    , bgBenches     :: [Bench a]
    , bgXAxisName   :: T.Text
    }
    deriving (Functor, Generic, Show, Foldable, Traversable)

instance Bin.Binary a => Bin.Binary (Bench a)
instance Bin.Binary a => Bin.Binary (BenchGroup a)

steps :: Num a => a -> a -> Int -> [a]
steps _ _ 0 = []
steps low stepSize n = low : steps (low + stepSize) stepSize (n - 1)

benchmarks = [
      let numIters = steps (10^7 :: Int) (10^6) 5 in
      BenchGroup {
        bgDescription = "countdown"
      , bgBenches = map (\(name, benchmark) ->
              Bench name (map (fromIntegral &&& whnf benchmark) numIters)
            )
            [("freer", Freer.countdown), ("mtl", Mtl.countdown)]
      , bgXAxisName = "# of iterations"
      }

    , BenchGroup {
        bgDescription = "readers above state"
      , bgBenches = (\n ->
          [
            Bench "mtl" (zip [1..8] [
                  whnf Mtl.readersAboveState1 n
                , whnf Mtl.readersAboveState2 n
                , whnf Mtl.readersAboveState3 n
                , whnf Mtl.readersAboveState4 n
                , whnf Mtl.readersAboveState5 n
                , whnf Mtl.readersAboveState6 n
                , whnf Mtl.readersAboveState7 n
                , whnf Mtl.readersAboveState8 n
                ])
          , Bench "freer" (zip [1..8] [
                whnf Freer.readersAboveState1 n
              , whnf Freer.readersAboveState2 n
              , whnf Freer.readersAboveState3 n
              , whnf Freer.readersAboveState4 n
              , whnf Freer.readersAboveState5 n
              , whnf Freer.readersAboveState6 n
              , whnf Freer.readersAboveState7 n
              , whnf Freer.readersAboveState8 n
              ])
          ]) (10^6)
      , bgXAxisName = "# of Reader layers above State"
      }
    ]

runBenchGroup :: BenchGroup Benchmarkable -> IO (BenchGroup Report)
runBenchGroup group = do
    putStrLn ("Benchmarking: " <> T.unpack (bgDescription group))
    mapM benchmark' group

getMean :: Report -> Double
getMean = estPoint . anMean . reportAnalysis

getPoints :: Bench Report -> [(Double, Double)]
getPoints = map (second getMean) . benchData

data Cycle a = Cycle { cycleValues :: [a], cycleCurrent :: Int }

newCycle :: [a] -> Cycle a
newCycle values = Cycle values 0

getCycleValue :: State.MonadState (Cycle a) m => m a
getCycleValue = do
    cycle <- State.get
    let next = cycleCurrent cycle + 1 `mod` length (cycleValues cycle)
    State.put (cycle { cycleCurrent = next })
    return (cycleValues cycle !! cycleCurrent cycle)

plotBench bench = do
    style <- getCycleValue
    return $ plot_points_values .~ getPoints bench
           $ plot_points_title .~ title
           $ plot_points_style .~ style
           $ def
  where
    title = T.unpack (benchDescription bench)

plotBenchGroup group = toRenderable layout
  where
    plots = State.evalState (mapM plotBench (bgBenches group)) (newCycle styles)

    xAxisName = T.unpack (bgXAxisName group)

    layout = layout_margin .~ 20
           $ layout_title .~ T.unpack (bgDescription group)
           $ layout_plots .~ map toPlot plots
           $ layout_x_axis . laxis_title .~ xAxisName
           $ layout_y_axis . laxis_title .~ "time (s)"
           $ def

    styles = map ($ point_radius .~ 5 $ def) [
          (point_color .~ opaque (Colour.sRGB24read "e41a1c")) .
          (point_shape .~ PointShapeCircle)
        , (point_color .~ opaque (Colour.sRGB24read "377eb8")) .
          (point_shape .~ PointShapePolygon 4 False)
        , (point_color .~ opaque (Colour.sRGB24read "4daf4a")) .
          (point_shape .~ PointShapePolygon 4 True)
        ]

data Options =
      Run {
          single :: Maybe T.Text
        , save   :: Maybe FilePath
        }
    | Plot FilePath
    deriving (Generic, Show)

instance Opts.ParseRecord Options

main :: IO ()
main = do
    opts <- Opts.getRecord "Benchmarking" :: IO Options
    case opts of
        Run mSingle mSave -> do
            let toBenchmark = case mSingle of
                                Just name -> maybeToList $ find ((== name) . bgDescription) benchmarks
                                Nothing -> benchmarks
            results <- mapM runBenchGroup toBenchmark

            case mSave of
                Just filename -> B.writeFile filename (Bin.encode results)
                Nothing -> return ()

        Main.Plot path -> do
            file <- B.readFile path
            let results = Bin.decode file :: [BenchGroup Report]
            mapM_ (\group ->
                renderableToFile
                    (def & fo_format .~ PDF)
                    (T.unpack (bgDescription group) <> ".pdf")
                    (plotBenchGroup group)
                ) results
