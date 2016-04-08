{-# LANGUAGE TypeFamilies #-}

module Extensible.Writer where

import           Control.Eff
import           Control.Eff.Writer.Lazy
import           Control.Monad           (replicateM_)


repeatedTell :: Int -> [Int]
repeatedTell n = fst $ run $ runMonoidWriter $ replicateM_ n (tell [0 :: Int])
