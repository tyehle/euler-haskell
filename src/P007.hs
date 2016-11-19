module P007 where

import Primes (primes)

run :: IO ()
run = print $ primes !! 10000
