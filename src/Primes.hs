module Primes where

import Data.List (group, tails, (\\))
import Data.Maybe (fromJust)
import Data.Foldable (find)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

factors :: Integer -> [Integer]
factors n = foldl (\factors pfs -> factors >>= (\factor -> map (factor *) pfs)) [1] pureFs
  where
    pureFs = map (map product . tails) . group . primeFactors $ n

divisors :: Integer -> [Integer]
divisors n = factors n \\ [n]

primeFactors :: Integer -> [Integer]
primeFactors n | n < 1     = undefined
               | n == 1    = []
               | otherwise = factor:primeFactors (n `div` factor)
  where
    factor = fromJust $ find (isDivisible n) primes

primesUnder :: Int -> [Int]
primesUnder n = sieve $ IntSet.fromAscList [2..n]
  where
    sieve remaining | IntSet.null remaining = []
                    | otherwise = let (prime, others) = IntSet.deleteFindMin remaining in
                                    prime : sieve (others `IntSet.difference` IntSet.fromAscList [prime, prime+prime .. n])

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
