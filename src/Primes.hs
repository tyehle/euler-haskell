module Primes
  ( divisors, factors, primeFactors
  , primes
  ) where

import Data.List (group, tails, (\\))
import Data.Maybe (fromJust)
import Data.Foldable (find)


divisors :: Integer -> [Integer]
divisors n = factors n \\ [n]

factors :: Integer -> [Integer]
factors n = foldl (\factors pfs -> factors >>= (\factor -> map (factor *) pfs)) [1] pureFs
  where
    pureFs = map (map product . tails) . group . primeFactors $ n

primeFactors :: Integer -> [Integer]
primeFactors n | n < 1     = undefined
               | n == 1    = []
               | otherwise = factor:primeFactors (n `div` factor)
  where
    factor = fromJust $ find (isDivisible n) primes
    isDivisible n f = n `mod` f == 0

-- define this operation on ordered lists
minus :: Ord a => [a] -> [a] -> [a]
minus (x:xs) (y:ys) = case compare x y of
           LT -> x : minus  xs  (y:ys)
           EQ ->     minus  xs     ys
           GT ->     minus (x:xs)  ys
minus  xs     _     = xs

primes :: [Integer]
primes = 2 : oddprimes
  where
    oddprimes = sieve [3,5..] 9 oddprimes
    sieve (x:xs) q ps@ ~(p:t)
      | x < q     = x : sieve xs q ps
      | otherwise =     sieve (xs `minus` [q, q+2*p..]) (head t^2) t
