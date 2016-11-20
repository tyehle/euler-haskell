module P021 where

import Primes (divisors)

run :: IO ()
run = print . sum . filter isAmicable $ [2..9999]

isAmicable :: Integer -> Bool
isAmicable a = a /= b && d b == a
  where
    d = sum . divisors
    b = d a
