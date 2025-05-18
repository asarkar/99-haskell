module List.P16 where

-- Problem 16: (**) Drop every N'th element from a list.
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = [x | (i, x) <- zip [1 ..] xs, i `mod` n /= 0]
