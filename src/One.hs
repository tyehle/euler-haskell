module One
    ( run
    ) where

run :: String
run = show result
  where
    result = sum $ filter (multipleOf [3, 5]) [1 .. 999]
    multipleOf factors n = any ((== 0) . mod n) factors
