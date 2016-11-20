module P024 where

run :: IO ()
run = putStrLn . concatMap show . (!! 999999) . permutations $ [0..9]

permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations xs = concatMap (\x -> map (x:) (permutations (without x xs))) xs
  where
    without n []     = []
    without n (x:xs) | n == x    = xs
                     | otherwise = x : without n xs
