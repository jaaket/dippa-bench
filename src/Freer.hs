{-# LANGUAGE FlexibleContexts #-}

module Freer where

import           Control.Monad.Freer
import           Control.Monad.Freer.State


countdown :: Int -> Int
countdown = fst . run . runState go
  where
    go :: Member (State Int) r => Eff r Int
    go = do
        x <- get
        if x == 0
            then return x
            else put (x - 1) >> go
