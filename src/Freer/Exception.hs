{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}

module Freer.Exception where

import           Control.Monad                 (foldM)
import           Control.Monad.Freer
import           Control.Monad.Freer.Exception


exception :: Int -> Either Int Int
exception n = run $ runError $ foldM f 1 (replicate n 1 ++ [0])
  where
    f _ x | x == 0 = throwError (0 :: Int)
    f acc x = return $! acc * x
