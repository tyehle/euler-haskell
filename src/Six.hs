module Six where

run :: String
run = show $ sum [1..100] ^ 2 - sum (map (^2) [1..100])
