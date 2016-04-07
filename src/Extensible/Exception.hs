{-# LANGUAGE TypeFamilies #-}

module Extensible.Exception where

import           Control.Eff
import           Control.Eff.Exception
import           Control.Monad         (foldM)


exception :: Int -> Either Int Int
exception n = run $ runExc $ foldM f 1 (replicate n 1 ++ [0])
  where
    f _ x | x == 0 = throwExc (0 :: Int)
    f acc x = return $! acc * x
