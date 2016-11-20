module P010 where

import Primes (primes)

run :: IO ()
run = print . sum . takeWhile (< 2000000) $ primes
