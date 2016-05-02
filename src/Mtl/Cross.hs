{-# LANGUAGE FlexibleContexts #-}

module Mtl.Cross where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.Writer.Strict


newtype S1 = S1 Int
newtype S2 = S2 Int

stateState :: Int -> Int
stateState n = flip evalState (S1 n) $ evalStateT go (S2 n)
  where
    go = do
        S1 x <- lift get
        S2 y <- get
        if x == 0
            then if y == 0
                then return y
                else put (S2 (y - 1)) >> go
            else lift (put (S1 (x - 1))) >> go

stateReader :: Int -> Int
stateReader n = flip runReader 0 $ evalStateT go n
  where
    go = do
        x <- get
        y <- ask
        if x == y
            then return x
            else put (x - 1) >> go

stateWriter :: Int -> Int
stateWriter n = getSum $ snd $ runWriter $ evalStateT go n
  where
    go = do
        x <- get
        tell (Sum x)
        if x == 0
            then return x
            else put (x - 1) >> go

stateException :: Int -> Either Int Int
stateException n = runExcept $ evalStateT go n
  where
    go = do
        x <- get
        if x == 0
            then throwError x
            else put (x - 1) >> go

readerState :: Int -> Int
readerState n = flip evalState n $ runReaderT go 0
  where
    go = do
        x <- get
        if x == 0
            then return x
            else put (x - 1) >> go

readerReader :: Int -> Int
readerReader n = flip runReader (S1 n) $ runReaderT go (S2 n)
  where
    go = do
        S1 x <- lift ask
        S2 y <- ask
        if x == 0
            then if y == 0
                then return y
                else local (\(S2 y) -> S2 (y - 1)) go
            else lift $ local (\(S1 x) -> S1 (x - 1)) (runReaderT go (S2 y))


readerWriter :: Int -> Int
readerWriter n = getSum $ snd $ runWriter $ runReaderT go n
  where
    go = do
        x <- ask
        tell (Sum x)
        if x == 0
            then return x
            else local (subtract (1 :: Int)) go
