{-# LANGUAGE FlexibleContexts #-}

module Extensible.StateWriter where

import          Control.Eff
import          Control.Eff.State.Lazy
import          Control.Eff.Writer.Lazy

countdownWrite :: (Member (State Int) r, Member (Writer [Int]) r) => Eff r Int
countdownWrite = do
    x <- get
    tell [x]
    if x == 0
        then return x
        else put (x - 1) >> countdownWrite

countdownWriterAbove :: Int -> [Int]
countdownWriterAbove n = fst $ snd $ run $ runState n $ runMonoidWriter countdownWrite
