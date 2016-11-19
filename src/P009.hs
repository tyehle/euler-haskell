module P009 where

import Data.Foldable (find)
import Data.Maybe (fromJust, mapMaybe)

run :: IO ()
run = print . mult $ triple
  where
    triple = fromJust . find (\(a,b,c) -> a+b+c == 1000) $ triples
    mult (a,b,c) = a*b*c

triples :: [(Integer, Integer, Integer)]
triples = mapMaybe maybeTriple . filter (uncurry (<)) $ pairs
  where
    maybeTriple (a,b) = case perfectRoot (a^2 + b^2) of
      Just c -> Just (a,b,c)
      Nothing -> Nothing

-- Will not work if the input is large enough
perfectRoot :: Integer -> Maybe Integer
perfectRoot n | root^2 == n = Just root
              | otherwise   = Nothing
  where
    root = floor . sqrt . fromIntegral $ n

pairs :: [(Integer, Integer)]
pairs = concatMap row [2..]
  where
    row total = map (\diff -> (diff, total-diff)) [1..total-1]
