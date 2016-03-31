{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
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
import           Data.List                       (transpose)
import qualified Data.Text                       as T
import           Statistics.Resampling.Bootstrap (Estimate (..))

import qualified Freer
import qualified Mtl


data Bench a = Bench {
      benchDescription :: T.Text
    , benchData        :: [a]
    }
    deriving (Functor, Show, Foldable, Traversable)

data BenchGroup a = BenchGroup {
      bgDescription :: T.Text
    , bgBenches     :: [Bench a]
    , bgXAxis       :: [Double]
    }
    deriving (Functor, Show, Foldable, Traversable)

countdown = BenchGroup {
      bgDescription = "countdown"
    , bgBenches = map (\(name, benchmark) ->
            Bench name (map (whnf benchmark) xValues)
          )
          [("freer", Freer.countdown), ("mtl", Mtl.countdown)]
    , bgXAxis = map fromIntegral xValues
    }
  where
    xValues = steps (10^7) (10^6) 5

runBenchGroup :: BenchGroup Benchmarkable -> IO (BenchGroup Report)
runBenchGroup = mapM benchmark'

getMean:: Report -> Double
getMean = estPoint . anMean . reportAnalysis

benchGroupCsv :: BenchGroup Report -> B.ByteString
benchGroupCsv (BenchGroup _ results xAxis) =
    Csv.encode ["time" : map benchDescription results] `mappend`
    Csv.encode (zipWith (:) xAxis (transpose $ map (map getMean . benchData) results))

steps :: Num a => a -> a -> Int -> [a]
steps _ _ 0 = []
steps low stepSize n = low : steps (low + stepSize) stepSize (n - 1)

main :: IO ()
main = do
    result <- runBenchGroup countdown
    B.writeFile "test.csv" (benchGroupCsv result)
