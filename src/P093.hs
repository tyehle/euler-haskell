module P093 where

import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ratio
import Data.List (tails, group, sort, elemIndex)

run :: IO ()
run = putStrLn . concatMap (show . numerator) . snd . maximum $ countPairs
  where
    countPairs = [(countSequential ds, ds) | ds <- digits]

countSequential :: [Rational] -> Int
countSequential ds = fromMaybe (length seqRes) . elemIndex False . zipWith (==) [1..] $ seqRes
  where
    seqRes = posResults ds

posResults :: [Rational] -> [Integer]
posResults ds = map head . group . sort . filter (> 0) $ integerResults
  where
    integerResults = mapMaybe toNum [runComp d op | d <- orderings ds, op <- ops ]

runComp :: [a] -> [a -> a -> Maybe a] -> Maybe a
runComp [] _ = undefined
runComp (x:xs) ops = foldl (\total (x,op) -> total >>= (`op` x)) (Just x) (zip xs ops)

orderings :: Eq a => [a] -> [[a]]
orderings [] = [[]]
orderings xs = [ x : rest | x <- xs, rest <- orderings (without x xs) ]
  where
    without n []     = []
    without n (x:xs) | n == x    = xs
                     | otherwise = x : without n xs

allChoices :: Int -> [a] -> [[a]]
allChoices 0 _ = [[]]
allChoices n xs = [ y:ys | y:rest <- tails xs
                         , ys <- allChoices (n-1) rest ]

allChoicesWithReplacement :: Int -> [a] -> [[a]]
allChoicesWithReplacement 0 _ = [[]]
allChoicesWithReplacement n xs = [ first : rest | first <- xs
                                                , rest <- allChoicesWithReplacement (n-1) xs]

digits :: [[Rational]]
digits = allChoices 4 [0..9]

ops :: [[Rational -> Rational -> Maybe Rational]]
ops = allChoicesWithReplacement 3 [wrapJust (+), wrapJust (-), wrapJust (*), maybeDiv]
  where
    wrapJust op a b = Just $ op a b
    maybeDiv a 0 = Nothing
    maybeDiv a b = Just $ a / b

toNum :: (Num a, Eq a) => Maybe (Ratio a) -> Maybe a
toNum Nothing = Nothing
toNum (Just r) | denominator r == 1 = Just $ numerator r
               | otherwise          = Nothing
