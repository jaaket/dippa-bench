{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Extensible.State where

import           Control.Monad (foldM)
import           Control.Eff
import           Control.Eff.Reader.Lazy
import           Control.Eff.State.Lazy


innerComputation :: Member (State Int) r => Int -> Eff r Int
innerComputation n = foldM f 1 [n,n-1..1] where
    f acc x | x `mod` 5 == 0 = do
                            s <- get
                            put $! (s + 1 :: Int)
                            return $! max acc x
    f acc x = return $! max acc x

-- Here, all the type annotations are necessary!

readersAboveState1 =
    run .
    runState (0 :: Int) .
    flip runReader (0 :: Int) .
    innerComputation
readersAboveState2 =
    run .
    runState (0 :: Int) .
    flip runReader (0 :: Int) . flip runReader (0 :: Int) .
    innerComputation
readersAboveState3 =
    run .
    runState (0 :: Int) .
    flip runReader (0 :: Int) . flip runReader (0 :: Int) . flip runReader (0 :: Int) .
    innerComputation
readersAboveState4 =
    run .
    runState (0 :: Int) .
    flip runReader (0 :: Int) . flip runReader (0 :: Int) . flip runReader (0 :: Int) . flip runReader (0 :: Int) .
    innerComputation
readersAboveState5 =
    run .
    runState (0 :: Int) .
    flip runReader (0 :: Int) . flip runReader (0 :: Int) . flip runReader (0 :: Int) . flip runReader (0 :: Int) . flip runReader (0 :: Int) .
    innerComputation

readersBelowState1 =
    run .
    flip runReader (0 :: Int) .
    runState (0 :: Int) .
    innerComputation
readersBelowState2 =
    run .
    flip runReader (0 :: Int) . flip runReader (0 :: Int) .
    runState (0 :: Int) .
    innerComputation
readersBelowState3 =
    run .
    flip runReader (0 :: Int) . flip runReader (0 :: Int). flip runReader (0 :: Int) .
    runState (0 :: Int) .
    innerComputation
readersBelowState4 =
    run .
    flip runReader (0 :: Int) . flip runReader (0 :: Int) . flip runReader (0 :: Int) . flip runReader (0 :: Int) .
    runState (0 :: Int) .
    innerComputation
readersBelowState5 =
    run .
    flip runReader (0 :: Int) . flip runReader (0 :: Int) . flip runReader (0 :: Int) . flip runReader (0 :: Int) . flip runReader (0 :: Int) .
    runState (0 :: Int) .
    innerComputation
