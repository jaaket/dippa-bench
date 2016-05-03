{-# LANGUAGE FlexibleContexts #-}

module Classes.Cross where

import           Control.Arrow (first)
import           Control.Monad (replicateM_)
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

stateWriterInner :: (MonadState Int m, MonadWriter (Sum Int) m) => m Int
stateWriterInner = do
    x <- get
    tell (Sum x)
    if x == 0
        then return x
        else put (x - 1 :: Int) >> stateWriterInner

stateWriter :: Int -> Int
stateWriter n =
    getSum $ snd $ run $ runWriterStrict $ evalStateStrict n stateWriterInner

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

readerWriterInner :: (MonadReader Int m, MonadLocal Int m, MonadWriter (Sum Int) m) => m Int
readerWriterInner = do
    x <- ask
    tell (Sum x)
    if x == (0 :: Int)
        then return x
        else local (subtract (1 :: Int)) readerWriterInner

readerWriter :: Int -> Int
readerWriter n =
    getSum $ snd $ run $ runWriterStrict $ runReader n readerWriterInner

readerException :: Int -> Either Int Int
readerException n = run $ runExcept $ runReader n go
  where
    go = do
        x <- ask
        if x == (0 :: Int)
            then throw x
            else local (subtract (1 :: Int)) go

writerState :: Int -> Int
writerState n =
    getSum $ snd $ run $ evalStateStrict n $ runWriterStrict stateWriterInner

writerReader :: Int -> Int
writerReader n =
    getSum $ snd $ run $ runReader n $ runWriterStrict readerWriterInner

writerWriter :: Int -> (Int, [Int])
writerWriter n = first (getSum . snd) $ run $ runWriterStrict $ runWriterStrict $
    replicateM_ n (tell (Sum (1 :: Int)) >> tell [1 :: Int])

writerException :: Int -> Either Int [Int]
writerException n = fmap snd $ run $ runExcept $ runWriterStrict $ do
    replicateM_ n (tell [1 :: Int])
    throw (0 :: Int)
