{-# LANGUAGE FlexibleContexts #-}

module Mtl.Writer where

import           Control.Monad.Writer.Lazy


repeatedTell :: Int -> [Int]
repeatedTell n = snd $ runWriter $ replicateM_ n (tell [0])
