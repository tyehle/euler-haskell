module Ten where

import Primes (primesUnder)

run :: String
run = show . sum . primesUnder $ 2000000
