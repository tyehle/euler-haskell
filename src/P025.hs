module P025 where

import Data.Maybe (fromJust)
import Data.List (find)
import Fibonacci (fibonacci)

run :: IO ()
run = print (fst found)
  where
    found = fromJust . find (\(i, f) -> (length . show) f == 1000) . zip [1..] $ fibonacci
