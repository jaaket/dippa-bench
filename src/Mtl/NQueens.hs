{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module Mtl.NQueens where

import           Control.Applicative (Alternative, empty, (<|>))
import           Control.Monad.Cont

import           Common.NQueens


choose :: MonadPlus m => [a] -> m a
choose = msum . map return

addQueens :: MonadPlus m => Int -> Int -> [(Int, Int)] -> m [(Int, Int)]
addQueens _ 0 qs = return qs
addQueens n col qs = do
    row <- choose (safePositionsOnColumn n col qs)
    addQueens n (col - 1) ((row, col) : qs)

instance Monad m => Alternative (ContT [r] m) where
    empty = ContT $ const (return [])
    (ContT f) <|> (ContT g) = ContT $ \k ->
        f k >>= \xs -> g k >>= \ys -> return (xs ++ ys)

instance Monad m => MonadPlus (ContT [r] m)

nQueens :: Int -> [[(Int, Int)]]
nQueens n =
    runCont (addQueens n n []) (\x -> [x])
