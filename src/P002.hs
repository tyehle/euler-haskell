module P002 where

run :: IO ()
run = print result
  where
    result = sum . filter even . takeWhile (< 4000000) $ fibFrom 1 2
    test = take 20 $ fibFrom 1 2

fibFrom :: Integer -> Integer -> [Integer]
fibFrom a b = a:b:remainingFibs a b
  where
    remainingFibs a b = a+b:remainingFibs b (a+b)
