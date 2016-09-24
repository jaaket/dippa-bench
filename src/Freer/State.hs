{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}

module Freer.State where

import           Control.Monad.Freer
import           Control.Monad.Freer.State


countdown :: Int -> Int
countdown = fst . run . runState go
  where
    go = do
        x <- get
        if x == 0
            then return x
            else put (x - 1) >> go
