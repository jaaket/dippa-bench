{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Freer.StateTH where

import           Control.Monad (foldM)
import           Control.Monad.Freer
import           Control.Monad.Freer.Reader
import           Control.Monad.Freer.State
import           Language.Haskell.TH


innerComputation :: Member (State Int) r => Int -> Eff r Int
innerComputation n = foldM f 1 [n,n-1..1] where
    f acc x | x `mod` 5 == 0 = do
                            s <- get
                            put $! (s + 1 :: Int)
                            return $! max acc x
    f acc x = return $! max acc x

genRunReaders :: Int -> Q Exp
genRunReaders n =
    foldr (`infixApp` [|(.)|]) [e|id|] (replicate n [|flip runReader 0|])

readersAboveStateClause :: Int -> Q Clause
readersAboveStateClause n = clause
    [litP (integerL (fromIntegral n))]
    (normalB [e|flip runState (0 :: Int) . $(genRunReaders n) . innerComputation|])
    []

readersBelowStateClause :: Int -> Q Clause
readersBelowStateClause n = clause
    [litP (integerL (fromIntegral n))]
    (normalB [e|$(genRunReaders n) . flip runState (0 :: Int) . innerComputation|])
    []

genReadersAboveState :: Int -> Q [Dec]
genReadersAboveState n = do
    f <- funD (mkName "readersAboveState") (map readersAboveStateClause [1..n])
    return [f]

genReadersBelowState :: Int -> Q [Dec]
genReadersBelowState n = do
    f <- funD (mkName "readersBelowState") (map readersBelowStateClause [1..n])
    return [f]
