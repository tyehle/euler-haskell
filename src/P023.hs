module P023 where

import Primes (divisors)

run :: IO ()
run = print . sum $ notSum
  where
    notSum = takeWhile (< 28123) . notSumOf $ smallAbundants
    smallAbundants = takeWhile (< 28123) abundants

notSumOf :: [Integer] -> [Integer]
notSumOf nums = sieve nums [1..]
  where
    sieve [] left = left
    sieve ns@(x:xs) left = sieve xs (left `minus` map (x+) ns)
    minus (x:xs) (y:ys) = case compare x y of
               LT -> x : minus  xs  (y:ys)
               EQ ->     minus  xs     ys
               GT ->     minus (x:xs)  ys
    minus  xs     _     = xs

abundants :: [Integer]
abundants = filter isAbundant [1..]
  where
    isAbundant n = (sum . divisors) n > n
