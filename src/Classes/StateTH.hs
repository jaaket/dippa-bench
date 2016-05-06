{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Classes.StateTH where

import Control.Monad (foldM)
import           Control.Monad.Classes
import           Control.Monad.Classes.Run
import           Language.Haskell.TH


innerComputation :: MonadState Int m => Int -> m Int
innerComputation n = foldM f 1 [1..n] where
    f acc x | x `mod` 5 == 0 = do
                            s <- get
                            put $! (s + 1 :: Int)
                            return $! max acc x
    f acc x = return $! max acc x


genRunReaders :: Int -> Q Exp
genRunReaders n =
    foldr (`infixApp` [|(.)|]) [e|id|] (replicate n [e|runReader 0|])

readersAboveStateClause :: Int -> Q Clause
readersAboveStateClause n = clause
    [litP (integerL (fromIntegral n))]
    (normalB [e|run . runStateStrict (0 :: Int) . $(genRunReaders n) . innerComputation|])
    []

readersBelowStateClause :: Int -> Q Clause
readersBelowStateClause n = clause
    [litP (integerL (fromIntegral n))]
    (normalB [e|run . $(genRunReaders n) . runStateStrict (0 :: Int) . innerComputation|])
    []

genReadersAboveState :: Int -> Q [Dec]
genReadersAboveState n = do
    f <- funD (mkName "readersAboveState") (map readersAboveStateClause [1..n])
    return [f]

genReadersBelowState :: Int -> Q [Dec]
genReadersBelowState n = do
    f <- funD (mkName "readersBelowState") (map readersBelowStateClause [1..n])
    return [f]
