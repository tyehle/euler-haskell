module Fifteen where

run :: String
run = show $ choose 40 20

choose :: Integer -> Integer -> Integer
choose n 0 = 1
choose 0 k = 0
choose n k = choose (n-1) (k-1) * n `div` k
