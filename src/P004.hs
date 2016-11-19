module P004 where

run :: IO ()
run = print result
  where
    factors = [100..999]
    products = concatMap (\a -> map (a*) factors) factors
    result = maximum . filter isPalindrome $ products

isPalindrome :: Integer -> Bool
isPalindrome n = show n == reverse (show n)
