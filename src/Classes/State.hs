{-# LANGUAGE FlexibleContexts #-}

module Classes.State where

import           Control.Monad.Classes
import           Control.Monad.Classes.Run


countdown :: Int -> Int
countdown = run . flip evalStateStrict go
  where
    go = do
        x <- get
        if x == 0
            then return x
            else put (x - 1) >> go
