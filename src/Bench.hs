{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Bench where

import           Control.Lens                    (set)
import           Control.Lens.Operators
import           Control.Lens.TH
import           Criterion
import           Criterion.Types                 (Regression (..), Report (..),
                                                  SampleAnalysis (..))
import qualified Data.Binary                     as Bin
import qualified Data.ByteString.Lazy            as B
import           Data.Csv
import qualified Data.Map.Lazy                   as Map
import           Data.Maybe                      (fromJust)
import           Data.Monoid                     ((<>))
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
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
    mapM benchAndRetry bench
  where
    benchAndRetry singleBench = do
        result <- benchmark' singleBench
        if getRSquared result < 0.99
            then putStrLn "R^2 < 0.99 => RETRY!" >> benchAndRetry singleBench
            else return result

getOLS :: Report -> Double
getOLS = estPoint . fromJust . Map.lookup "iters" . regCoeffs . head . anRegress . reportAnalysis

getRSquared :: Report -> Double
getRSquared = estPoint . regRSquare . head . anRegress . reportAnalysis

getMean :: Report -> Double
getMean = estPoint . anMean . reportAnalysis

getHeader :: BenchGroup a -> [T.Text]
getHeader group = ["framework", group ^. bgXAxisName, "time"]

getMeasurements :: Bench a -> [Measurement a]
getMeasurements bench =
    map (\(x, y) -> Measurement (bench ^. benchDescription) x y)
        (bench ^. benchData)

data Measurement a = Measurement {
      _mLabel  :: T.Text
    , _mXValue :: Double
    , _mYValue :: a
    }
    deriving (Functor, Generic, Show)

makeLenses ''Measurement

instance ToField a => ToRecord (Measurement a)

toCsv :: ToField a => BenchGroup a -> B.ByteString
toCsv group =
       B.intercalate "," (map (B.fromStrict . T.encodeUtf8) (getHeader group))
    <> "\n"
    <> encode (concatMap getMeasurements (group ^. bgBenches))
