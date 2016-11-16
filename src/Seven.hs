module Seven where

import Primes (primes)

run :: String
run = show $ primes !! 10000
