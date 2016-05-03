{-# LANGUAGE FlexibleContexts #-}

module Freer.Cross where

import           Control.Arrow (first)
import           Control.Monad (replicateM_)
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

stateWriterInner :: (Member (State Int) r, Member (Writer (Sum Int)) r)
                 => Eff r Int
stateWriterInner = do
    x <- get
    tell (Sum (x :: Int))
    if x == 0
        then return x
        else put (x - 1) >> stateWriterInner

stateWriter :: Int -> Int
stateWriter n = getSum $ snd $ run $ runWriter $ runState stateWriterInner n

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

readerWriterInner :: (Member (Reader Int) r, Member (Writer (Sum Int)) r)
                  => Eff r Int
readerWriterInner = do
    x <- ask
    tell (Sum (x :: Int))
    if x == 0
        then return x
        else local (subtract (1 :: Int)) readerWriterInner

readerWriter :: Int -> Int
readerWriter n = getSum $ snd $ run $ runWriter $ runReader readerWriterInner n

readerException :: Int -> Either Int Int
readerException n = run $ runError $ runReader go n
  where
    go = do
        x <- ask
        if x == (0 :: Int)
            then throwError x
            else local (subtract (1 :: Int)) go

writerState :: Int -> Int
writerState n =
    getSum $ snd $ fst $ run $ flip runState n $ runWriter stateWriterInner

writerReader :: Int -> Int
writerReader n =
    getSum $ snd $ run $ flip runReader n $ runWriter readerWriterInner

writerWriter :: Int -> (Int, [Int])
writerWriter n = first (getSum . snd) $ run $ runWriter $ runWriter $
    replicateM_ n (tell (Sum (1 :: Int)) >> tell [1 :: Int])
