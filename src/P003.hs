module P003 where

import Primes (primeFactors)

run :: IO ()
run = print result
  where
    n = 600851475143
    result = last . primeFactors $ n
