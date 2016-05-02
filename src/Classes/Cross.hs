{-# LANGUAGE FlexibleContexts #-}

module Classes.Cross where

import           Control.Monad.Classes
import           Control.Monad.Classes.Run
import           Data.Monoid


newtype S1 = S1 Int
newtype S2 = S2 Int

stateState :: Int -> Int
stateState n = run $ evalStateStrict (S1 n) $ evalStateStrict (S2 n) go
  where
    go = do
        S1 x <- get
        S2 y <- get
        if x == 0
            then if y == 0
                then return y
                else put (S2 (y - 1)) >> go
            else put (S1 (x - 1)) >> go

newtype Env = Env { getEnv :: Int }

stateReader :: Int -> Int
stateReader n = run $ runReader (Env 0) $ evalStateStrict n go
  where
    go = do
        x <- get
        Env y <- ask
        if x == y
            then return x
            else put (x - 1) >> go

stateWriter :: Int -> Int
stateWriter n = getSum $ snd $ run $ runWriterStrict $ evalStateStrict n go
  where
    go = do
        x <- get
        tell (Sum x)
        if x == 0
            then return x
            else put (x - 1 :: Int) >> go

stateException :: Int -> Either Int Int
stateException n = run $ runExcept $ evalStateStrict n go
  where
    go = do
        x <- get
        if x == 0
            then throw x
            else put (x - 1 :: Int) >> go

readerState :: Int -> Int
readerState n = run $ evalStateStrict n $ runReader (0 :: Int) go
  where
    go = do
        x <- get
        y <- ask
        if x == y
            then return x
            else put (x - 1) >> go

readerReader :: Int -> Int
readerReader n = run $ runReader (S1 n) $ runReader (S2 n) go
  where
    go = do
        S1 x <- ask
        S2 y <- ask
        if x == 0
            then if y == 0
                then return y
                else local (\(S2 y) -> S2 (y - 1)) go
            else local (\(S1 x) -> S1 (x - 1)) go

readerWriter :: Int -> Int
readerWriter n = getSum $ snd $ run $ runWriterStrict $ runReader n go
  where
    go :: (MonadReader Int m, MonadLocal Int m, MonadWriter (Sum Int) m) => m Int
    go = do
        x <- ask
        tell (Sum x)
        if x == 0
            then return x
            else local (subtract (1 :: Int)) go
