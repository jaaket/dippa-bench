{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Criterion
import           Criterion.Types                 (Report (..),
                                                  SampleAnalysis (..))
import qualified Data.ByteString.Lazy            as B
import qualified Data.Csv                        as Csv
import qualified Data.Text                       as T
import           GHC.Generics                    (Generic)
import           Statistics.Resampling.Bootstrap (Estimate (..))

import qualified Freer
import qualified Mtl


data BenchElem label info = BenchElem {
      elemLabel :: label
    , elemInfo  :: info
    }
    deriving (Functor, Show, Generic)

data Bench a = Bench {
      benchDescription :: T.Text
    , benchData        :: [a]
    }
    deriving (Functor, Show, Foldable, Traversable)

data BenchGroup a = BenchGroup {
      bgDescription :: T.Text
    , bgBenches     :: [Bench a]
    }
    deriving (Functor, Show, Foldable, Traversable)

countdown = BenchGroup {
      bgDescription = "countdown"
    , bgBenches = map (\(name, benchmark) ->
            Bench name (map (\n -> BenchElem n (whnf benchmark n)) xValues)
          )
          [("freer", Freer.countdown), ("mtl", Mtl.countdown)]
    }
  where
    xValues = steps (10^3) (10^2) 3

runBench :: BenchElem a Benchmarkable -> IO (BenchElem a Report)
runBench (BenchElem label bench) = BenchElem label <$> benchmark' bench

getPlotData :: BenchElem a Report -> BenchElem a Double
getPlotData = fmap (estPoint . anMean . reportAnalysis)

data BenchRow a b = BenchRow T.Text a b deriving (Show, Generic)

instance (Csv.ToField a, Csv.ToField b) => Csv.ToRecord (BenchRow a b)

benchRows :: Bench (BenchElem a b) -> [BenchRow a b]
benchRows (Bench desc rows) = map (\(BenchElem x y) -> BenchRow desc x y) rows

benchGroupCsv :: Csv.ToField a => BenchGroup (BenchElem a Report) -> B.ByteString
benchGroupCsv (BenchGroup _ benches) =
    Csv.encode (concatMap (benchRows . fmap getPlotData) benches)

steps :: Num a => a -> a -> Int -> [a]
steps _ _ 0 = []
steps low stepSize n = low : steps (low + stepSize) stepSize (n - 1)

main :: IO ()
main = do
    result <- mapM runBench countdown
    print (benchGroupCsv result)
