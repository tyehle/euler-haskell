module Three where

import Primes (primeFactors)

run :: String
run = show result
  where
    n = 600851475143
    result = last . primeFactors $ n
