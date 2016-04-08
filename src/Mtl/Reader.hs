{-# LANGUAGE FlexibleContexts #-}

module Mtl.Reader where

import           Control.Monad.Reader


countdownReader :: Int -> Int
countdownReader = runReader go
  where
    go = do
        x <- ask
        if x == 0
            then return x
            else local (subtract 1) go
