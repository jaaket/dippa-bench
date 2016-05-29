{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}

module Bench where

import           Criterion
import           Criterion.Types                 (Regression (..), Report (..),
                                                  SampleAnalysis (..))
import qualified Data.Binary                     as Bin
import qualified Data.Map.Lazy                   as Map
import           Data.Maybe                      (fromJust)
import           Data.Monoid                     ((<>))
import qualified Data.Text                       as T
import           GHC.Generics                    (Generic)
import           Statistics.Resampling.Bootstrap (Estimate (..))


data Bench a = Bench {
      benchDescription :: T.Text
    , benchData        :: [(Double, a)]
    }
    deriving (Functor, Generic, Show, Foldable, Traversable)

data BenchGroup a = BenchGroup {
      bgId          :: T.Text
    , bgDescription :: T.Text
    , bgBenches     :: [Bench a]
    , bgXAxisName   :: T.Text
    }
    deriving (Functor, Generic, Show, Foldable, Traversable)

instance Bin.Binary a => Bin.Binary (Bench a)
instance Bin.Binary a => Bin.Binary (BenchGroup a)

runBenchGroup :: BenchGroup Benchmarkable -> IO (BenchGroup Report)
runBenchGroup group = do
    putStrLn $ "Benchmarking group: " <> T.unpack (bgDescription group) <>
               " (" <> T.unpack (bgId group) <> ")"
    results <- mapM describeAndRun (bgBenches group)
    return $ group { bgBenches = results }

describeAndRun :: Bench Benchmarkable -> IO (Bench Report)
describeAndRun bench = do
    putStrLn $ "Running benchmark: " <> T.unpack (benchDescription bench)
    mapM benchmark' bench

getOLS :: Report -> Double
getOLS = estPoint . fromJust . Map.lookup "iters" . regCoeffs . head . anRegress . reportAnalysis

getMean :: Report -> Double
getMean = estPoint . anMean . reportAnalysis
