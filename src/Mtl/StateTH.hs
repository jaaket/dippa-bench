{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Mtl.StateTH where

import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Language.Haskell.TH


innerComputation :: MonadState Int m => Int -> m Int
innerComputation n = foldM f 1 [1..n] where
    f acc x | x `mod` 5 == 0 = do
                            s <- get
                            put $! (s + 1 :: Int)
                            return $! max acc x
    f acc x = return $! max acc x

genRunReaderTs :: Int -> Q Exp
genRunReaderTs n =
    foldr (`infixApp` [|(.)|]) [e|id|] (replicate n [|flip runReaderT 0|])

readersAboveStateClause :: Int -> Q Clause
readersAboveStateClause n = clause
    [litP (integerL (fromIntegral n))]
    (normalB [e|flip runState 0 . $(genRunReaderTs n) . innerComputation|])
    []

readersBelowStateClause :: Int -> Q Clause
readersBelowStateClause n = clause
    [litP (integerL (fromIntegral n))]
    (normalB [e|flip runReader 0 . $(genRunReaderTs (n - 1)) . flip runStateT 0 . innerComputation|])
    []

genReadersAboveState :: Int -> Q [Dec]
genReadersAboveState n = do
    f <- funD (mkName "readersAboveState") (map readersAboveStateClause [0..n])
    return [f]

genReadersBelowState :: Int -> Q [Dec]
genReadersBelowState n = do
    f <- funD (mkName "readersBelowState") (map readersBelowStateClause [0..n])
    return [f]
