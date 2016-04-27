{-# LANGUAGE FlexibleContexts #-}

module Mtl.Writer where

import           Control.Monad.Writer.Strict


repeatedTell :: Int -> [Int]
repeatedTell n = snd $ runWriter $ replicateM_ n (tell [0])
