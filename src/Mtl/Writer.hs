{-# LANGUAGE FlexibleContexts #-}

module Mtl.Writer where

import           Control.Monad.Writer.Strict


repeatedTell :: Int -> Int
repeatedTell n = getSum $ snd $ runWriter $ replicateM_ n (tell (Sum 1))
