{-# LANGUAGE FlexibleContexts #-}

module Classes.Exception where

import           Control.Monad             (foldM)
import           Control.Monad.Classes
import           Control.Monad.Classes.Run


exception :: Int -> Either Int Int
exception n = run $ runExcept $ foldM f 1 (replicate n 1 ++ [0])
  where
    f _ x | x == 0 = throw (0 :: Int)
    f acc x = return $! acc * x
