{-# LANGUAGE FlexibleContexts #-}

module Classes.Reader where

import           Control.Monad.Classes
import           Control.Monad.Classes.Run


countdownReader :: Int -> Int
countdownReader n = run $ runReader n go
  where
    go = do
        x <- ask
        if x == 0
            then return x
            else local (subtract (1 :: Int)) go
