{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Extensible.NQueens where

import           Control.Eff
import           Control.Eff.Choose

import           Common.NQueens


addQueens :: Member Choose r => Int -> Int -> [(Int, Int)] -> Eff r [(Int, Int)] -- optional
addQueens _ 0 qs = return qs
addQueens n col qs = do
    row <- choose (safePositionsOnColumn n col qs)
    addQueens n (col - 1) ((row, col) : qs)

nQueens :: Int -> [[(Int, Int)]]
nQueens n =
    run (runChoice (addQueens n n []))
