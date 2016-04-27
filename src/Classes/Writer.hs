{-# LANGUAGE FlexibleContexts #-}

module Classes.Writer where

import           Control.Monad             (replicateM_)
import           Control.Monad.Classes
import           Control.Monad.Classes.Run


repeatedTell :: Int -> [Int]
repeatedTell n = snd $ run $ runWriterStrict $ replicateM_ n (tell [0 :: Int])
