module P010 where

import Primes (primesUnder)

run :: IO ()
run = print . sum . primesUnder $ 2000000
