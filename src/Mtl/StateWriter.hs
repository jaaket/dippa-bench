{-# LANGUAGE FlexibleContexts #-}

module Mtl.StateWriter where

import           Control.Monad.State.Lazy
import           Control.Monad.Writer.Lazy


countdownWrite :: (MonadState Int m, MonadWriter [Int] m) => m Int
countdownWrite = do
    x <- get
    tell [x]
    if x == 0
        then return x
        else put (x - 1) >> countdownWrite

countdownWriterAbove :: Int -> [Int]
countdownWriterAbove n = snd $ evalState (runWriterT countdownWrite) n

countdownWriterBelow :: Int -> [Int]
countdownWriterBelow n = snd $ runWriter (evalStateT countdownWrite n)
