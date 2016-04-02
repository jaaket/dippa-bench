{-# LANGUAGE FlexibleContexts #-}

module Freer where

import           Control.Monad (foldM)
import           Control.Monad.Freer
import           Control.Monad.Freer.Reader
import           Control.Monad.Freer.State


countdown :: Int -> Int
countdown = fst . run . runState go
  where
    go = do
        x <- get
        if x == 0
            then return x
            else put (x - 1) >> go

innerComputation :: Member (State Int) r => Int -> Eff r Int
innerComputation n = foldM f 1 [n,n-1..1] where
    f acc x | x `mod` 5 == 0 = do
                            s <- get
                            put $! (s + 1 :: Int)
                            return $! max acc x
    f acc x = return $! max acc x

wrapReader m = runReader m 0
wrapReader2 = wrapReader . wrapReader
wrapReader3 = wrapReader . wrapReader2
wrapReader4 = wrapReader . wrapReader3
wrapReader5 = wrapReader . wrapReader4
wrapReader6 = wrapReader . wrapReader5
wrapReader7 = wrapReader . wrapReader6
wrapReader8 = wrapReader . wrapReader7

readersAboveState t = run . flip runState 0 . t . innerComputation

readersAboveState1 :: Int -> (Int, Int)
readersAboveState1 = readersAboveState wrapReader

readersAboveState2 :: Int -> (Int, Int)
readersAboveState2 = readersAboveState wrapReader2

readersAboveState3 :: Int -> (Int, Int)
readersAboveState3 = readersAboveState wrapReader3

readersAboveState4 :: Int -> (Int, Int)
readersAboveState4 = readersAboveState wrapReader4

readersAboveState5 :: Int -> (Int, Int)
readersAboveState5 = readersAboveState wrapReader5

readersAboveState6 :: Int -> (Int, Int)
readersAboveState6 = readersAboveState wrapReader6

readersAboveState7 :: Int -> (Int, Int)
readersAboveState7 = readersAboveState wrapReader7

readersAboveState8 :: Int -> (Int, Int)
readersAboveState8 = readersAboveState wrapReader8
