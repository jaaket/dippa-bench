{-# LANGUAGE FlexibleContexts #-}

module Classes.Cross where

import           Control.Monad.Classes
import           Control.Monad.Classes.Run

stateReader :: Int -> Int
stateReader n = run $ runReader 0 $ evalStateStrict n go
  where
    go = do
        x <- get
        if x == 0
            then return x
            else put (x - 1 :: Int) >> go

stateWriter :: Int -> [Int]
stateWriter n = snd $ run $ runWriterStrict $ evalStateStrict n go
  where
    go = do
        x <- get
        tell [x]
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
readerState n = run $ evalStateStrict n $ runReader 0 go
  where
    go = do
        x <- get
        if x == 0
            then return x
            else put (x - 1) >> go
