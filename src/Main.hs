{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Arrow                          ((&&&))
import           Control.Monad                          (when)
import           Criterion
import           Criterion.Types                        (Report)
import qualified Data.Binary                            as Bin
import qualified Data.ByteString.Lazy                   as B
import           Data.Maybe                             (fromMaybe)
import           Data.Monoid                            ((<>))
import qualified Data.Text                              as T
import           GHC.Generics                           (Generic)
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy
import qualified Options.Generic                        as Opts

import           Bench
import qualified Effects.Countdown                      as Effects
import qualified Effects.NQueens                        as Effects
import qualified Extensible.Countdown                   as Extensible
import qualified Extensible.Exception                   as Extensible
import qualified Extensible.NQueens                     as Extensible
import qualified Extensible.State                       as Extensible
import qualified Freer.Countdown                        as Freer
import qualified Freer.Exception                        as Freer
import qualified Freer.NQueens                          as Freer
import qualified Freer.State                            as Freer
import qualified Mtl.Countdown                          as Mtl
import qualified Mtl.Exception                          as Mtl
import qualified Mtl.NQueens                            as Mtl
import qualified Mtl.State                              as Mtl
import           Plot


steps :: Num a => a -> a -> Int -> [a]
steps _ _ 0 = []
steps low stepSize n = low : steps (low + stepSize) stepSize (n - 1)

benchmarks = [
      let numIters = steps (10^6) (10^6) 5 in
      BenchGroup {
            bgDescription = "countdown"
          , bgBenches = map (\(name, benchmark) ->
                  Bench name (map (fromIntegral &&& whnf benchmark) numIters)
                )
                [("effects", Effects.countdown), ("extensible-effects", Extensible.countdown), ("freer", Freer.countdown), ("mtl", Mtl.countdown)]
          , bgXAxisName = "# of iterations"
      }

    , BenchGroup {
        bgDescription = "readers above state"
      , bgBenches = (\n ->
          [
            Bench "mtl" (zip [1..5] [
                  whnf Mtl.readersAboveState1 n
                , whnf Mtl.readersAboveState2 n
                , whnf Mtl.readersAboveState3 n
                , whnf Mtl.readersAboveState4 n
                , whnf Mtl.readersAboveState5 n
                ])
          , Bench "freer" (zip [1..5] [
                whnf Freer.readersAboveState1 n
              , whnf Freer.readersAboveState2 n
              , whnf Freer.readersAboveState3 n
              , whnf Freer.readersAboveState4 n
              , whnf Freer.readersAboveState5 n
              ])
          , Bench "extensible-effects" (zip [1..5] [
                whnf Extensible.readersAboveState1 n
              , whnf Extensible.readersAboveState2 n
              , whnf Extensible.readersAboveState3 n
              , whnf Extensible.readersAboveState4 n
              , whnf Extensible.readersAboveState5 n
              ])
          ]) (10^6)
      , bgXAxisName = "# of Reader layers above State"
      }

    , BenchGroup {
          bgDescription = "readers below state"
        , bgBenches = (\n ->
            [
              Bench "mtl" (zip [1..5] [
                    whnf Mtl.readersBelowState1 n
                  , whnf Mtl.readersBelowState2 n
                  , whnf Mtl.readersBelowState3 n
                  , whnf Mtl.readersBelowState4 n
                  , whnf Mtl.readersBelowState5 n
                  ])
            , Bench "freer" (zip [1..5] [
                  whnf Freer.readersBelowState1 n
                , whnf Freer.readersBelowState2 n
                , whnf Freer.readersBelowState3 n
                , whnf Freer.readersBelowState4 n
                , whnf Freer.readersBelowState5 n
                ])
            , Bench "extensible-effects" (zip [1..5] [
                  whnf Extensible.readersBelowState1 n
                , whnf Extensible.readersBelowState2 n
                , whnf Extensible.readersBelowState3 n
                , whnf Extensible.readersBelowState4 n
                , whnf Extensible.readersBelowState5 n
                ])
            ]) (10^6)
        , bgXAxisName = "# of Reader layers below State"
        }

    , let numIters = steps (10^6) (10^6) 5 in
      BenchGroup {
          bgDescription = "exception"
        , bgBenches = map (\(name, benchmark) ->
                Bench name (map (fromIntegral &&& whnf benchmark) numIters)
              )
              [("extensible-effects", Extensible.exception), ("freer", Freer.exception), ("mtl", Mtl.exception)]
        , bgXAxisName = "# of iterations"
        }

    , BenchGroup {
          bgDescription = "n-queens"
        , bgBenches = map (\(name, benchmark) ->
                Bench name (map (fromIntegral &&& whnf benchmark) [6..10])
              )
              [("effects", Effects.nQueens), ("extensible-effects", Extensible.nQueens), ("freer", Freer.nQueens), ("mtl", Mtl.nQueens)]
        , bgXAxisName = "n"
        }
    ]

data Options =
      Run {
          bench    :: [T.Text]
        , save     :: Bool
        , savePath :: Maybe FilePath
        }
    | Plot FilePath
    deriving (Generic, Show)

instance Opts.ParseRecord Options

main :: IO ()
main = do
    opts <- Opts.getRecord "Benchmarking" :: IO Options
    case opts of
        Run benches save mSavePath -> do
            let toBenchmark = filter ((`elem` benches) . bgDescription) benchmarks
            results <- mapM runBenchGroup toBenchmark

            when save $
                let filename = fromMaybe "results.bin" mSavePath in
                    B.writeFile filename (Bin.encode results)

        Main.Plot path -> do
            file <- B.readFile path
            let results = Bin.decode file :: [BenchGroup Report]
            mapM_ (\group ->
                renderableToFile
                    (def & fo_format .~ PDF)
                    (T.unpack (bgDescription group) <> ".pdf")
                    (plotBenchGroup group)
                ) results
