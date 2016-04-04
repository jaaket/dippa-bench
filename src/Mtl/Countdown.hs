{-# LANGUAGE FlexibleContexts #-}

module Mtl.Countdown where

import           Control.Monad.State


countdown :: Int -> Int
countdown = evalState go
  where
    go = do
        x <- get
        if x == 0
            then return x
            else put (x - 1) >> go
