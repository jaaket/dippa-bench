{-# LANGUAGE DeriveGeneric #-}

module Regression where

import           Data.Csv
import qualified Data.Text    as T
import           GHC.Generics (Generic)


data Regression = Regression {
      regBench  :: T.Text
    , regFw     :: T.Text
    , regR2     :: Double
    , regCoeff  :: Double
    , regDegree :: Int
    }
    deriving (Generic, Show)

instance FromRecord Regression
