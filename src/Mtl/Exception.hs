{-# LANGUAGE FlexibleContexts #-}

module Mtl.Exception where

import           Control.Monad.Except


exception :: Int -> Either Int Int
exception n = foldM f 1 (replicate n 1 ++ [0])
  where
    f _ x | x == 0 = throwError 0
    f acc x = return $! acc * x
