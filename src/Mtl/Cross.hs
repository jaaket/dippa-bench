{-# LANGUAGE FlexibleContexts #-}

module Mtl.Cross where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.Writer.Strict

stateReader :: Int -> Int
stateReader n = flip runReader 0 $ evalStateT go n
  where
    go = do
        x <- get
        if x == 0
            then return x
            else put (x - 1) >> go

stateWriter :: Int -> [Int]
stateWriter n = snd $ runWriter $ evalStateT go n
  where
    go = do
        x <- get
        tell [x]
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
