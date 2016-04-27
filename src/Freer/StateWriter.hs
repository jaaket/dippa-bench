{-# LANGUAGE FlexibleContexts #-}

module Freer.StateWriter where

import          Control.Monad.Freer
import          Control.Monad.Freer.State
import          Control.Monad.Freer.Writer

countdownWrite :: (Member (State Int) r, Member (Writer [Int]) r) => Eff r Int
countdownWrite = do
    x <- get
    tell [x]
    if x == 0
        then return x
        else put (x - 1) >> countdownWrite

countdownWriterAbove :: Int -> [Int]
countdownWriterAbove n = snd $ fst $ run $ flip runState n $ runWriter countdownWrite

countdownWriterBelow :: Int -> [Int]
countdownWriterBelow n = snd $ run $ runWriter $ runState countdownWrite n
