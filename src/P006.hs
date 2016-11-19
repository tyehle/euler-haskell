module P006 where

run :: IO ()
run = print $ sum [1..100] ^ 2 - sum (map (^2) [1..100])
