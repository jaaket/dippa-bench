{-# LANGUAGE FlexibleContexts #-}

module Mtl.State where

import           Control.Monad.State.Strict


countdown :: Int -> Int
countdown = evalState go
  where
    go = do
        x <- get
        if x == 0
            then return x
            else put (x - 1) >> go
