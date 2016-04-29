{-# LANGUAGE FlexibleContexts #-}

module Classes.StateWriter where

import           Control.Monad.Classes
import           Control.Monad.Classes.Run


countdownWrite :: (MonadState Int m, MonadWriter [Int] m) => m Int
countdownWrite = do
    x <- get
    tell [x]
    if x == 0
        then return x
        else put (x - 1) >> countdownWrite

countdownWriterAbove :: Int -> [Int]
countdownWriterAbove n = snd $ fst $ run $ runStateStrict n $ runWriterStrict countdownWrite

countdownWriterBelow :: Int -> [Int]
countdownWriterBelow n = snd $ run $ runWriterStrict $ evalStateStrict n countdownWrite
