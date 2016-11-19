module P001 where

run :: IO ()
run = print result
  where
    result = sum $ filter (multipleOf [3, 5]) [1 .. 999]
    multipleOf factors n = any ((== 0) . mod n) factors
