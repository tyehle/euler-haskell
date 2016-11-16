module Three where

import Data.Foldable (find)

run :: String
run = show result
  where
    n = 600851475143
    result = last . primeFactors $ n

primeFactors :: Integer -> [Integer]
primeFactors n | n <= 1    = []
               | otherwise = factor:primeFactors (n `div` factor)
  where
    factor = case find (isDivisible n) primes of
      Just prime -> prime
      _ -> error "Halting problem solved. Collect your turing award."

primes :: [Integer]
primes = 2:remainingPrimes [2]

remainingPrimes :: [Integer] -> [Integer]
remainingPrimes primes = next : remainingPrimes (next:primes)
  where next = nextPrime primes

nextPrime :: [Integer] -> Integer
nextPrime primes@(largest:others) = case find (isPrime primes) [largest+1..] of
  Just prime -> prime
  _ -> error $ "No primes past "++show largest++". Check in for your fields medal."

isPrime :: [Integer] -> Integer -> Bool
isPrime reverseSmallerPrimes n = all (not . isDivisible n) $ dropWhile (> maxFactor) reverseSmallerPrimes
  where
    maxFactor = floor . sqrt . fromInteger $ n

isDivisible :: Integer -> Integer -> Bool
isDivisible n f = n `mod` f == 0
