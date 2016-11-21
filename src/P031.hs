module P031 where

import Data.List (sort)
import Caching (memoize)

run :: IO ()
run = print . length $ findChange 200 [1, 2, 5, 10, 20, 50, 100, 200]

findChange :: Integer -> [Integer] -> [[Integer]]
findChange target unsortedCoins = cachedChange (target, length unsortedCoins)
  where
    coins = undefined : sort unsortedCoins
    bounds = ((0, 0), (target, length coins))
    cachedChange = memoize changeStep bounds
    changeStep (total, nCoins) | total == 0 = [replicate nCoins 0]
                               | nCoins == 0 = []
                               | otherwise = let dontUse = map (0:) $ cachedChange (total, nCoins - 1)
                                             in if total - (coins !! nCoins) < 0
                                                then dontUse
                                                else dontUse ++ map (\(c:cs) -> c+1:cs) (cachedChange (total - (coins !! nCoins), nCoins))
