{-# LANGUAGE FlexibleContexts #-}

module Mtl where

import           Control.Monad.State


countdown :: Int -> Int
countdown = evalState go
  where
    go :: MonadState Int m => m Int
    go = do
        x <- get
        if x == 0
            then return x
            else put (x - 1) >> go
