module P002 where

import Fibonacci (fibFrom)

run :: IO ()
run = print result
  where
    result = sum . filter even . takeWhile (< 4000000) $ fibFrom 1 2
    test = take 20 $ fibFrom 1 2
