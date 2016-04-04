module Common.NQueens where


noAttack :: (Int, Int) -> (Int, Int) -> Bool
noAttack (x, y) (x', y') =
    x /= x' && y /= y' && abs (x - x') /= abs (y - y')

safePositionsOnColumn :: Int -> Int -> [(Int, Int)] -> [Int]
safePositionsOnColumn n col qs =
    [ x | x <- [1..n], all (noAttack (x, col)) qs ]
