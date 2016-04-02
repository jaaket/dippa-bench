{-# LANGUAGE FlexibleContexts #-}

module Mtl where

import           Control.Monad.Reader
import           Control.Monad.State


countdown :: Int -> Int
countdown = evalState go
  where
    go = do
        x <- get
        if x == 0
            then return x
            else put (x - 1) >> go

wrapReader m = runReaderT m 0
wrapReader2 = wrapReader . wrapReader
wrapReader3 = wrapReader . wrapReader2
wrapReader4 = wrapReader . wrapReader3
wrapReader5 = wrapReader . wrapReader4
wrapReader6 = wrapReader . wrapReader5
wrapReader7 = wrapReader . wrapReader6
wrapReader8 = wrapReader . wrapReader7

innerComputation :: MonadState Int m => Int -> m Int
innerComputation n = foldM f 1 [1..n] where
    f acc x | x `mod` 5 == 0 = do
                            s <- get
                            put $! (s + 1 :: Int)
                            return $! max acc x
    f acc x = return $! max acc x

readersAboveState t = flip runState 0 . t . innerComputation

readersAboveState1 = readersAboveState wrapReader
readersAboveState2 = readersAboveState wrapReader2
readersAboveState3 = readersAboveState wrapReader3
readersAboveState4 = readersAboveState wrapReader4
readersAboveState5 = readersAboveState wrapReader5
readersAboveState6 = readersAboveState wrapReader6
readersAboveState7 = readersAboveState wrapReader7
readersAboveState8 = readersAboveState wrapReader8
