module Primes where

import Data.Maybe (fromJust)
import Data.Foldable (find)

primeFactors :: Integer -> [Integer]
primeFactors n | n <= 1    = []
               | otherwise = factor:primeFactors (n `div` factor)
  where
    factor = fromJust $ find (isDivisible n) primes

primes :: [Integer]
primes = 2:remainingPrimes [2]

remainingPrimes :: [Integer] -> [Integer]
remainingPrimes primes = next : remainingPrimes (next:primes)
  where next = nextPrime primes

nextPrime :: [Integer] -> Integer
nextPrime primes@(largest:others) = fromJust $ find (isPrime primes) [largest+1..]

isPrime :: [Integer] -> Integer -> Bool
isPrime reverseSmallerPrimes n = all (not . isDivisible n) $ dropWhile (> maxFactor) reverseSmallerPrimes
  where
    maxFactor = floor . sqrt . fromInteger $ n

isDivisible :: Integer -> Integer -> Bool
isDivisible n f = n `mod` f == 0
