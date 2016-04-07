{-# LANGUAGE FlexibleContexts #-}

module Classes.Countdown where

import           Control.Monad.Classes
import           Control.Monad.Classes.Run


countdown :: Int -> Int
countdown n = run $ evalStateLazy n go
  where
    go = do
        x <- get
        if x == 0
            then return x
            else put (x - 1) >> go
