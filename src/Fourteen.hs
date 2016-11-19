module Fourteen where

import Data.Foldable (maximumBy)

run :: String
run = show result
  where
    result = fst . maximumBy (\a b -> snd a `compare` snd b) . map (\n -> (n, collatzLength n)) $ [1..1000000]

collatzLength :: Integer -> Integer
collatzLength n | n < 1     = undefined
                | n == 1    = 1
                | even n    = 1 + collatzLength (n `div` 2)
                | otherwise = 1 + collatzLength (3*n + 1)
