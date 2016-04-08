{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}

module Bench where

import           Criterion
import           Criterion.Types (Report (..))
import qualified Data.Binary     as Bin
import           Data.Monoid     ((<>))
import qualified Data.Text       as T
import           GHC.Generics    (Generic)



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
    putStrLn ("Benchmarking: " <> T.unpack (bgDescription group))
    mapM benchmark' group
