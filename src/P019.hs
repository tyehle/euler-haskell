module P019 where

-- Dates advance one weekday every year except
-- on leap years when they stay the same

run :: IO ()
run = print result
  where
    days = scanl stepYear dow1901 [1902..2000]
    result = length . filter (0 ==) . concat $ days

stepYear :: [Integer] -> Integer -> [Integer]
stepYear lastFirsts year
  | isLeap year     = doLeapYear
  | isLeap (year-1) = doPrevLeapYear
  | otherwise       = doYear
  where
    isLeap yr = (yr `mod` 4 == 0) && (yr `mod` 100 /= 0 || yr `mod` 400 == 0)
    doPrevLeapYear = map leapDay (take 2 lastFirsts) ++ map advanceDay (drop 2 lastFirsts)
    doLeapYear = map advanceDay (take 2 lastFirsts) ++ map leapDay (drop 2 lastFirsts)
    doYear = map advanceDay lastFirsts
    advanceDay d = (d+1) `mod` 7
    leapDay d = (d+2) `mod` 7

dow1901 :: [Integer]
dow1901 = scanl nextFirst 2 numDays
  where
    nextFirst d m = (d+m) `mod` 7
    numDays = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30]
