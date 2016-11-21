module Fibonacci where

fibonacci :: [Integer]
fibonacci = fibFrom 1 1
  
fibFrom :: Integer -> Integer -> [Integer]
fibFrom a b = a:b:remainingFibs a b
  where
    remainingFibs a b = a+b:remainingFibs b (a+b)
