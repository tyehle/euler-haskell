module Seventeen where

run :: String
run = show . length . filter (/= '-') . filter (/= ' ') . concatMap asWords $ [1..1000]

asWords :: Integer -> String
asWords n | n >= 100000 = undefined
          | n > 999 = asWords (n `div` 1000) ++ " thousand" ++ doLower (n `mod` 1000)
          | n > 99  = asWords (n `div` 100) ++ " hundred" ++ doLower (n `mod` 100)
          | n > 89 = lastTwo "ninety" n
          | n > 79 = lastTwo "eighty" n
          | n > 69 = lastTwo "seventy" n
          | n > 59 = lastTwo "sixty" n
          | n > 49 = lastTwo "fifty" n
          | n > 39 = lastTwo "forty" n
          | n > 29 = lastTwo "thirty" n
          | n > 19 = lastTwo "twenty" n
          | n == 19 = "nineteen"
          | n == 18 = "eighteen"
          | n == 17 = "seventeen"
          | n == 16 = "sixteen"
          | n == 15 = "fifteen"
          | n == 14 = "fourteen"
          | n == 13 = "thirteen"
          | n == 12 = "twelve"
          | n == 11 = "eleven"
          | n == 10 = "ten"
          | n == 9  = "nine"
          | n == 8  = "eight"
          | n == 7  = "seven"
          | n == 6  = "six"
          | n == 5  = "five"
          | n == 4  = "four"
          | n == 3  = "three"
          | n == 2  = "two"
          | n == 1  = "one"
          | n == 0  = "zero"
          | n < 0 = "negative " ++ asWords (negate n)
  where
    lastTwo tens l = tens ++ afterTens (l `mod` 10)
    afterTens 0 = ""
    afterTens l = '-' : asWords l
    doLower l | l == 0    = ""
              | l < 100   = " and " ++ asWords l
              | otherwise = ' ' : asWords (l `div` 100 * 100) ++ doLower (l `mod` 100)
