{-# LANGUAGE FlexibleContexts #-}

module Effects.Countdown where

import           Control.Effects
import           Control.Effects.State


countdown :: Int -> Int
countdown n = run $ with (ref n) go
  where
    go loc = do
        x <- get loc
        if x == 0
            then return x
            else put loc (x - 1) >> go loc
