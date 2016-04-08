module Extensible.Reader where

import           Control.Eff
import           Control.Eff.Reader.Lazy


countdownReader :: Int -> Int
countdownReader n = run $ runReader go n
  where
    go = do
        x <- ask
        if x == 0
            then return x
            else local (subtract (1 :: Int)) go
