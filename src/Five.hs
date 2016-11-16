module Five where

import Primes (primes)

import Data.Maybe (fromJust)
import Data.Foldable (find)

run :: String
run = show result
  where
    high = 20
    relavantPrimes = takeWhile (<= high) primes
    maxPower base = fromJust (find (\power -> base^power > high) [1..]) - 1
    factors = concatMap (\p -> replicate (maxPower p) p) relavantPrimes
    result = product factors
