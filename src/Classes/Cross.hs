{-# LANGUAGE FlexibleContexts #-}

module Classes.Cross where

import           Control.Arrow (first)
import           Control.Monad (foldM, replicateM_)
import           Control.Monad.Classes
import           Control.Monad.Classes.Run
import           Data.Monoid

import           Common.ErrCode


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

stateExceptionInner :: (MonadState Int m, MonadExcept ErrCode m) => m Int
stateExceptionInner = do
    x <- get
    if x == 0
        then throw (ErrCode x)
        else put (x - 1 :: Int) >> stateExceptionInner

stateException :: Int -> Either ErrCode Int
stateException n = run $ runExcept $ evalStateStrict n stateExceptionInner

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

readerExceptionInner :: (MonadReader Int m, MonadLocal Int m, MonadExcept ErrCode m)
                     => m Int
readerExceptionInner = do
    x <- ask
    if x == (0 :: Int)
        then throw (ErrCode x)
        else local (subtract (1 :: Int)) readerExceptionInner

readerException :: Int -> Either ErrCode Int
readerException n = run $ runExcept $ runReader n readerExceptionInner

writerState :: Int -> Int
writerState n =
    getSum $ snd $ run $ evalStateStrict n $ runWriterStrict stateWriterInner

writerReader :: Int -> Int
writerReader n =
    getSum $ snd $ run $ runReader n $ runWriterStrict readerWriterInner

writerWriter :: Int -> (Int, [Int])
writerWriter n = first (getSum . snd) $ run $ runWriterStrict $ runWriterStrict $
    replicateM_ n (tell (Sum (1 :: Int)) >> tell [1 :: Int])

writerExceptionInner :: (MonadWriter [Int] m, MonadExcept ErrCode m)
                     => Int -> m Int
writerExceptionInner n = do
    replicateM_ n (tell [1 :: Int])
    throw (ErrCode 0)

writerException :: Int -> Either ErrCode [Int]
writerException n =
    run $ runExcept $ execWriterStrict $ writerExceptionInner n

exceptionState :: Int -> Either ErrCode Int
exceptionState n = run $ evalStateStrict n $ runExcept stateExceptionInner

exceptionReader :: Int -> Either ErrCode Int
exceptionReader n = run $ runReader n $ runExcept readerExceptionInner

exceptionWriter :: Int -> Either ErrCode Int
exceptionWriter n = fst helper
  where
    -- GHC needs help choosing monoid instance
    helper :: (Either ErrCode Int, [Int])
    helper = run $ runWriterStrict $ runExcept $ writerExceptionInner n

exceptionInner :: MonadExcept ErrCode m => Int -> m Int
exceptionInner n = foldM f 1 (replicate n 1 ++ [0])
  where
    f _ x | x == 0 = throw (ErrCode 0)
    f acc x = return $! acc * x

exceptionException :: Int -> Either String (Either ErrCode Int)
exceptionException n = run $ runExcept $ runExcept $ do
    exceptionInner n
    throw "error"
