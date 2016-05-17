{-# LANGUAGE FlexibleContexts #-}

module Freer.Cross where

import           Control.Arrow (first)
import           Control.Monad (foldM, replicateM_)
import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.Reader
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Writer
import           Data.Monoid

import           Common.ErrCode


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

stateReaderInner :: (Member (Reader Int) r, Member (State Int) r)
                 => Eff r Int
stateReaderInner = do
    x <- get
    y <- ask
    if x == (0 :: Int)
        then return y
        else put (x - 1) >> stateReaderInner

stateReader :: Int -> Int
stateReader n =
    snd $ run $ flip runReader (0 :: Int) $ runState stateReaderInner n

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

stateExceptionInner :: (Member (State Int) r, Member (Exc ErrCode) r)
                    => Eff r Int
stateExceptionInner = do
    x <- get
    if x == 0
        then throwError (ErrCode x)
        else put (x - 1 :: Int) >> stateExceptionInner

stateException :: Int -> Either ErrCode Int
stateException n = fmap snd $ run $ runError $ runState stateExceptionInner n

readerState :: Int -> Int
readerState n =
    fst $ run $ flip runState n $ runReader stateReaderInner (0 :: Int)

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

readerExceptionInner :: (Member (Reader Int) r, Member (Exc ErrCode) r)
                     => Eff r Int
readerExceptionInner = do
    x <- ask
    if x == (0 :: Int)
        then throwError (ErrCode x)
        else local (subtract (1 :: Int)) readerExceptionInner

readerException :: Int -> Either ErrCode Int
readerException n = run $ runError $ runReader readerExceptionInner n

writerState :: Int -> Int
writerState n =
    getSum $ snd $ fst $ run $ flip runState n $ runWriter stateWriterInner

writerReader :: Int -> Int
writerReader n =
    getSum $ snd $ run $ flip runReader n $ runWriter readerWriterInner

writerWriter :: Int -> (Int, [Int])
writerWriter n = first (getSum . snd) $ run $ runWriter $ runWriter $
    replicateM_ n (tell (Sum (1 :: Int)) >> tell [1 :: Int])

writerExceptionInner :: (Member (Writer [Int]) r, Member (Exc ErrCode) r)
                     => Int -> Eff r Int
writerExceptionInner n = do
    replicateM_ n (tell [1 :: Int])
    throwError (ErrCode 0)

writerException :: Int -> Either ErrCode Int
writerException n = fst <$> helper
  where
    -- GHC needs help choosing monoid instance
    helper :: Either ErrCode (Int, [Int])
    helper = run $ runError $ runWriter $ writerExceptionInner n

exceptionState :: Int -> Either ErrCode Int
exceptionState n =
    fst $ run $ flip runState n $ runError stateExceptionInner

exceptionReader :: Int -> Either ErrCode Int
exceptionReader n = run $ flip runReader n $ runError readerExceptionInner

exceptionWriter :: Int -> Either ErrCode Int
exceptionWriter n = fst helper
  where
    -- GHC needs help choosing monoid instance
    helper :: (Either ErrCode Int, [Int])
    helper = run $ runWriter $ runError $ writerExceptionInner n

exceptionInner :: Member (Exc ErrCode) r  => Int -> Eff r Int
exceptionInner n = foldM f 1 (replicate n 1 ++ [0])
  where
    f _ x | x == 0 = throwError (ErrCode 0)
    f acc x = return $! acc * x

exceptionException :: Int -> Either String (Either ErrCode Int)
exceptionException n = run $ runError $ runError $ do
    exceptionInner n
    throwError "error"
