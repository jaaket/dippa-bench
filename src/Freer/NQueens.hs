{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Freer.NQueens where

import Control.Monad (msum, MonadPlus)
import Control.Monad.Freer.Internal

import Common.NQueens


data Nondet a x where
    Choose :: [a] -> Nondet a a

choose :: Member (Nondet a) r => [a] -> Eff r a
choose = send . Choose

runNondet :: MonadPlus m => Eff (Nondet a ': r) w -> Eff r (m w)
runNondet = handleRelay
    (return . return)
    (\(Choose choices) k -> msum <$> mapM k choices)

addQueens :: Member (Nondet Int) r => Int -> Int -> [(Int, Int)] -> Eff r [(Int, Int)]
addQueens _ 0 qs = return qs
addQueens n col qs = do
    row <- choose (safePositionsOnColumn n col qs)
    addQueens n (col - 1) ((row, col) : qs)

nQueens :: Int -> [[(Int, Int)]]
nQueens n =
    run (runNondet (addQueens n n [] :: Eff (Nondet Int ': r) [(Int, Int)]))
