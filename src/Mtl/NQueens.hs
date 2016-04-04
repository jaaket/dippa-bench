{-# LANGUAGE FlexibleContexts #-}

module Mtl.NQueens where

import           Control.Monad.Amb

import           Common.NQueens


addQueens :: Int -> Int -> [(Int, Int)] -> Amb [(Int, Int)] [(Int, Int)]
addQueens _ 0 qs = return qs
addQueens n col qs = do
    row <- amb (safePositionsOnColumn n col qs)
    addQueens n (col - 1) ((row, col) : qs)

nQueens :: Int -> [[(Int, Int)]]
nQueens n =
    allValues (addQueens n n [])
