{-# LANGUAGE FlexibleContexts #-}

module Classes.State where

import Control.Monad (foldM)
import           Control.Monad.Classes
import           Control.Monad.Classes.Run


innerComputation :: MonadState Int m => Int -> m Int
innerComputation n = foldM f 1 [1..n] where
    f acc x | x `mod` 5 == 0 = do
                            s <- get
                            put $! (s + 1 :: Int)
                            return $! max acc x
    f acc x = return $! max acc x

readersAboveState1 =
    run .
    runStateLazy (0 :: Int) .
    runReader 0 .
    innerComputation
readersAboveState2 =
    run .
    runStateLazy (0 :: Int) .
    runReader 0 . runReader 0 .
    innerComputation
readersAboveState3 =
    run .
    runStateLazy (0 :: Int) .
    runReader 0 . runReader 0 . runReader 0 .
    innerComputation
readersAboveState4 =
    run .
    runStateLazy (0 :: Int) .
    runReader 0 . runReader 0 . runReader 0 . runReader 0 .
    innerComputation
readersAboveState5 =
    run .
    runStateLazy (0 :: Int) .
    runReader 0 . runReader 0 . runReader 0 . runReader 0 . runReader 0 .
    innerComputation

readersBelowState1 =
    run .
    runReader 0 .
    runStateLazy (0 :: Int) .
    innerComputation
readersBelowState2 =
    run .
    runReader 0 . runReader 0 .
    runStateLazy (0 :: Int) .
    innerComputation
readersBelowState3 =
    run .
    runReader 0 . runReader 0 . runReader 0 .
    runStateLazy (0 :: Int) .
    innerComputation
readersBelowState4 =
    run .
    runReader 0 . runReader 0 . runReader 0 . runReader 0 .
    runStateLazy (0 :: Int) .
    innerComputation
readersBelowState5 =
    run .
    runReader 0 . runReader 0 . runReader 0 . runReader 0 . runReader 0 .
    runStateLazy (0 :: Int) .
    innerComputation
