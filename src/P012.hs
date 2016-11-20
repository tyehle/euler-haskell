module P012 where

import Primes (factors)

import Data.Foldable (find)
import Data.Maybe (fromJust)

run :: IO ()
run = print . fromJust . find ((> 500) . length . factors) $ triangleNumbers

triangleNumbers :: [Integer]
triangleNumbers = remaining 1 2
  where
    remaining this n = this : remaining (this+n) (n+1)
