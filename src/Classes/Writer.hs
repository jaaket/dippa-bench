{-# LANGUAGE FlexibleContexts #-}

module Classes.Writer where

import           Control.Monad             (replicateM_)
import           Control.Monad.Classes
import           Control.Monad.Classes.Run
import           Data.Monoid               (Sum (..), getSum)


repeatedTell :: Int -> Int
repeatedTell n =
    getSum $ snd $ run $ runWriterStrict $ replicateM_ n (tell (Sum (1 :: Int)))
