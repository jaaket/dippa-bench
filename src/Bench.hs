{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell   #-}

module Bench where

import           Control.Lens                    (set)
import           Control.Lens.Operators
import           Control.Lens.TH
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
      _benchDescription :: T.Text
    , _benchData        :: [(Double, a)]
    }
    deriving (Functor, Generic, Show, Foldable, Traversable)

makeLenses ''Bench

data BenchGroup a = BenchGroup {
      _bgId          :: T.Text
    , _bgDescription :: T.Text
    , _bgBenches     :: [Bench a]
    , _bgXAxisName   :: T.Text
    }
    deriving (Functor, Generic, Show, Foldable, Traversable)

makeLenses ''BenchGroup

instance Bin.Binary a => Bin.Binary (Bench a)
instance Bin.Binary a => Bin.Binary (BenchGroup a)

runBenchGroup :: BenchGroup Benchmarkable -> IO (BenchGroup Report)
runBenchGroup group = do
    putStrLn $ "Benchmarking group: " <> T.unpack (group ^. bgDescription) <>
               " (" <> T.unpack (group ^. bgId) <> ")"
    results <- mapM describeAndRun (group ^. bgBenches)
    return $ set bgBenches results group

describeAndRun :: Bench Benchmarkable -> IO (Bench Report)
describeAndRun bench = do
    putStrLn $ "Running benchmark: " <> T.unpack (bench ^. benchDescription)
    mapM benchmark' bench

getOLS :: Report -> Double
getOLS = estPoint . fromJust . Map.lookup "iters" . regCoeffs . head . anRegress . reportAnalysis

getMean :: Report -> Double
getMean = estPoint . anMean . reportAnalysis
