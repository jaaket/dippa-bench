module Effects.NQueens where

import           Control.Effects
import           Control.Effects.NonDet

import           Common.NQueens


addQueens _ 0 qs _ = return qs
addQueens n col qs choice = do
    row <- choose choice (safePositionsOnColumn n col qs)
    addQueens n (col - 1) ((row, col) : qs) choice

nQueens :: Int -> [[(Int, Int)]]
nQueens n =
    run $ with alternatives (addQueens n n [])
