{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Freer.Writer where

import           Control.Monad              (replicateM_)
import           Control.Monad.Freer
import           Control.Monad.Freer.Writer
import           Data.Monoid                (Sum (..), getSum)


repeatedTell :: Int -> Int
repeatedTell n =
    getSum $ snd $ run $ runWriter $ replicateM_ n (tell (Sum (1 :: Int)))
