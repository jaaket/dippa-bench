{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Freer.Writer where

import           Control.Monad              (replicateM_)
import           Control.Monad.Freer
import           Control.Monad.Freer.Writer


repeatedTell :: Int -> [Int]
repeatedTell n = snd $ run $ runWriter $ replicateM_ n (tell (0 :: Int))
