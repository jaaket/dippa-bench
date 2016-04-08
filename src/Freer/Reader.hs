module Freer.Reader where

import           Control.Monad.Freer
import           Control.Monad.Freer.Reader

countdownReader :: Int -> Int
countdownReader n = run $ runReader go n
  where
    go = do
        x <- ask
        if x == 0
            then return x
            else local (subtract (1 :: Int)) go
