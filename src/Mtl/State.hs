{-# LANGUAGE FlexibleContexts #-}

module Mtl.State where

import           Control.Monad.Reader
import           Control.Monad.State.Strict


innerComputation :: MonadState Int m => Int -> m Int
innerComputation n = foldM f 1 [1..n] where
    f acc x | x `mod` 5 == 0 = do
                            s <- get
                            put $! (s + 1 :: Int)
                            return $! max acc x
    f acc x = return $! max acc x

readersAboveState1 =
    flip runState 0 .
    flip runReaderT 0 .
    innerComputation
readersAboveState2 =
    flip runState 0 .
    flip runReaderT 0 . flip runReaderT 0 .
    innerComputation
readersAboveState3 =
    flip runState 0 .
    flip runReaderT 0 . flip runReaderT 0 . flip runReaderT 0 .
    innerComputation
readersAboveState4 =
    flip runState 0 .
    flip runReaderT 0 . flip runReaderT 0 . flip runReaderT 0 .
    flip runReaderT 0 .
    innerComputation
readersAboveState5 =
    flip runState 0 .
    flip runReaderT 0 . flip runReaderT 0 . flip runReaderT 0 .
    flip runReaderT 0 . flip runReaderT 0 .
    innerComputation

readersBelowState1 =
    flip runReader 0 .
    flip runStateT 0 .
    innerComputation
readersBelowState2 =
    flip runReader 0 . flip runReaderT 0 .
    flip runStateT 0 .
    innerComputation
readersBelowState3 =
    flip runReader 0 . flip runReaderT 0 . flip runReaderT 0 .
    flip runStateT 0 .
    innerComputation
readersBelowState4 =
    flip runReader 0 . flip runReaderT 0 . flip runReaderT 0 .
    flip runReaderT 0 .
    flip runStateT 0 .
    innerComputation
readersBelowState5 =
    flip runReader 0 . flip runReaderT 0 . flip runReaderT 0 .
    flip runReaderT 0 . flip runReaderT 0 .
    flip runStateT 0 .
    innerComputation
