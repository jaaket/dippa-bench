{-# LANGUAGE FlexibleContexts #-}

module Freer.State where

import           Control.Monad (foldM)
import           Control.Monad.Freer
import           Control.Monad.Freer.Reader
import           Control.Monad.Freer.State


innerComputation :: Member (State Int) r => Int -> Eff r Int
innerComputation n = foldM f 1 [n,n-1..1] where
    f acc x | x `mod` 5 == 0 = do
                            s <- get
                            put $! (s + 1 :: Int)
                            return $! max acc x
    f acc x = return $! max acc x

readersAboveState1 =
    run .
    flip runState (0 :: Int) .
    flip runReader 0 .
    innerComputation
readersAboveState2 =
    run .
    flip runState (0 :: Int) .
    flip runReader 0 . flip runReader 0 .
    innerComputation
readersAboveState3 =
    run .
    flip runState (0 :: Int) .
    flip runReader 0 . flip runReader 0 . flip runReader 0 .
    innerComputation
readersAboveState4 =
    run .
    flip runState (0 :: Int) .
    flip runReader 0 . flip runReader 0 . flip runReader 0 . flip runReader 0 .
    innerComputation
readersAboveState5 =
    run .
    flip runState (0 :: Int) .
    flip runReader 0 . flip runReader 0 . flip runReader 0 . flip runReader 0 .
    flip runReader 0 .
    innerComputation

readersBelowState1 =
    run .
    flip runReader 0 .
    flip runState (0 :: Int) .
    innerComputation
readersBelowState2 =
    run .
    flip runReader 0 . flip runReader 0 .
    flip runState (0 :: Int) .
    innerComputation
readersBelowState3 =
    run .
    flip runReader 0 . flip runReader 0 . flip runReader 0 .
    flip runState (0 :: Int) .
    innerComputation
readersBelowState4 =
    run .
    flip runReader 0 . flip runReader 0 . flip runReader 0 . flip runReader 0 .
    flip runState (0 :: Int) .
    innerComputation
readersBelowState5 =
    run .
    flip runReader 0 . flip runReader 0 . flip runReader 0 . flip runReader 0 .
    flip runReader 0 .
    flip runState (0 :: Int) .
    innerComputation
