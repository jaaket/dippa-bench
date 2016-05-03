{-# LANGUAGE DeriveGeneric #-}

module Common.ErrCode where

import           Control.DeepSeq
import           GHC.Generics    (Generic)


newtype ErrCode = ErrCode Int deriving (Generic, Show)

instance NFData ErrCode
