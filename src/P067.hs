module P067 where

run :: IO ()
run = numsFromFile "resources/p067_triangle.txt" >>= print . result

result :: [[Integer]] -> Integer
result nums = head $ foldr (\line totals -> zipWith (+) line (bestOfChildren totals)) (repeat 0) nums
  where
    bestOfChildren line = zipWith max line (tail line)

numsFromFile :: String -> IO [[Integer]]
numsFromFile file = readFile file >>= pure . map (map read . words) . lines
