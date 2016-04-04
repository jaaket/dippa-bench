{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Freer.NQueens where

import Control.Monad (msum, MonadPlus)
import Control.Monad.Freer.Internal

data Nondet a x where
    Choose :: [a] -> Nondet a a

choose :: Member (Nondet a) r => [a] -> Eff r a
choose = send . Choose

runNondet :: MonadPlus m => Eff (Nondet a ': r) w -> Eff r (m w)
runNondet = handleRelay
    (return . return)
    (\(Choose choices) k -> msum <$> mapM k choices)

test :: Member (Nondet Int) r => Eff r (Int, Int)
test = do
    x <- choose [1,2,3]
    y <- choose [4,5]
    return (x, y)

runTest :: [(Int, Int)]
runTest = run (runNondet (test :: Eff (Nondet Int ': r) (Int, Int)))
