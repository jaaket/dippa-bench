module Extensible.Countdown where

import           Control.Eff
import           Control.Eff.State.Lazy


countdown :: Int -> Int
countdown = fst . run . flip runState go
  where
    go = do
        x <- get
        if x == 0
            then return x
            else put (x - 1 :: Int) >> go
