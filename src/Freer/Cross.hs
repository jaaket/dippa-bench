module Freer.Cross where

import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.Reader
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Writer
import           Data.Monoid


newtype S1 = S1 Int
newtype S2 = S2 Int

stateState :: Int -> Int
stateState n = fst $ fst $ run $ flip runState (S1 n) $ runState go (S2 n)
  where
    go = do
        S1 x <- get
        S2 y <- get
        if x == 0
            then if y == 0
                then return y
                else put (S2 (y - 1)) >> go
            else put (S1 (x - 1)) >> go

stateReader :: Int -> Int
stateReader n = snd $ run $ flip runReader (0 :: Int) $ runState go n
  where
    go = do
        x <- get
        y <- ask
        if x == y
            then return x
            else put (x - 1 :: Int) >> go

stateWriter :: Int -> Int
stateWriter n = getSum $ snd $ run $ runWriter $ runState go n
  where
    go = do
        x <- get
        tell (Sum (x :: Int))
        if x == 0
            then return x
            else put (x - 1) >> go

stateException :: Int -> Either Int Int
stateException n = fmap snd $ run $ runError $ runState go n
  where
    go = do
        x <- get
        if x == 0
            then throwError x
            else put (x - 1 :: Int) >> go

readerState :: Int -> Int
readerState n = fst $ run $ flip runState n $ runReader go 0
  where
    go = do
        x <- get
        if x == 0
            then return x
            else put (x - 1) >> go

readerReader :: Int -> Int
readerReader n = run $ flip runReader (S1 n) $ runReader go (S2 n)
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
readerWriter n = getSum $ snd $ run $ runWriter $ runReader go n
  where
    go = do
        x <- ask
        tell (Sum (x :: Int))
        if x == 0
            then return x
            else local (subtract (1 :: Int)) go