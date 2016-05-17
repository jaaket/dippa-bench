{-# LANGUAGE FlexibleContexts #-}

module Mtl.Cross where

import           Control.Arrow (first)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.Writer.Strict

import           Common.ErrCode


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

stateReaderInner :: (MonadState Int m, MonadReader Int m) => m Int
stateReaderInner = do
    x <- get
    y <- ask
    if x == 0
        then return y
        else put (x - 1) >> stateReaderInner

stateReader :: Int -> Int
stateReader n = flip runReader 0 $ evalStateT stateReaderInner n

stateWriterInner :: (MonadState Int m, MonadWriter (Sum Int) m) => m Int
stateWriterInner = do
    x <- get
    tell (Sum x)
    if x == 0
        then return x
        else put (x - 1) >> stateWriterInner

stateWriter :: Int -> Int
stateWriter n = getSum $ snd $ runWriter $ evalStateT stateWriterInner n

stateExceptionInner :: (MonadState Int m, MonadError ErrCode m) => m Int
stateExceptionInner = do
    x <- get
    if x == 0
        then throwError (ErrCode x)
        else put (x - 1) >> stateExceptionInner

stateException :: Int -> Either ErrCode Int
stateException n = runExcept $ evalStateT stateExceptionInner n

readerState :: Int -> Int
readerState n = flip evalState n $ runReaderT stateReaderInner 0

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

readerWriterInner :: (MonadReader Int m, MonadWriter (Sum Int) m) => m Int
readerWriterInner = do
    x <- ask
    tell (Sum x)
    if x == 0
        then return x
        else local (subtract (1 :: Int)) readerWriterInner

readerWriter :: Int -> Int
readerWriter n = getSum $ snd $ runWriter $ runReaderT readerWriterInner n

readerExceptionInner :: (MonadReader Int m, MonadError ErrCode m) => m Int
readerExceptionInner = do
    x <- ask
    if x == 0
        then throwError (ErrCode x)
        else local (subtract 1) readerExceptionInner

readerException :: Int -> Either ErrCode Int
readerException n = runExcept $ runReaderT readerExceptionInner n

writerState :: Int -> Int
writerState n = getSum $ snd $ flip evalState n $ runWriterT stateWriterInner

writerReader :: Int -> Int
writerReader n = getSum $ snd $ flip runReader n $ runWriterT readerWriterInner

writerWriter :: Int -> (Int, [Int])
writerWriter n = first (getSum . snd) $ runWriter $ runWriterT $
    replicateM_ n (tell (Sum 1) >> lift (tell [1]))

writerExceptionInner :: (MonadWriter [Int] m, MonadError ErrCode m)
                     => Int -> m Int
writerExceptionInner n = do
    replicateM_ n (tell [1])
    throwError (ErrCode 0)

writerException :: Int -> Either ErrCode Int
writerException n =
    fmap fst $ runExcept $ runWriterT $ writerExceptionInner n

exceptionState :: Int -> Either ErrCode Int
exceptionState n = flip evalState n $ runExceptT stateExceptionInner

exceptionReader :: Int -> Either ErrCode Int
exceptionReader n = flip runReader n $ runExceptT readerExceptionInner

exceptionWriter :: Int -> Either ErrCode Int
exceptionWriter n = fst $ runWriter $ runExceptT $ writerExceptionInner n

exceptionInner :: MonadError ErrCode m => Int -> m Int
exceptionInner n = foldM f 1 (replicate n 1 ++ [0])
  where
    f _ x | x == 0 = throwError (ErrCode 0)
    f acc x = return $! acc * x

exceptionException :: Int -> Either String (Either ErrCode Int)
exceptionException n = runExcept $ runExceptT $ do
    exceptionInner n
    lift (throwError "error")
