module Arrays where

import Data.List (transpose)

window :: Int -> [a] -> [[a]]
window n xs | length xs >= n = take n xs : window n (drop 1 xs)
            | otherwise      = []

verticalWindow :: Int -> [[a]] -> [[a]]
verticalWindow size grid = concatMap transpose rowGroups
  where
    rowGroups = window size grid

horizontalWindow :: Int -> [[a]] -> [[a]]
horizontalWindow size = concatMap (window size)

forwardDiagWindow :: Int -> [[a]] -> [[a]]
forwardDiagWindow size grid = concatMap (transpose . skew) rowGroups
  where
    rowGroups = window size grid
    skew = map (\(off,row) -> (take (length row - size + 1) . drop off) row) . zip [size-1,size-2..0]

backwardDiagWindow :: Int -> [[a]] -> [[a]]
backwardDiagWindow size grid = concatMap (transpose . skew) rowGroups
  where
    rowGroups = window size grid
    skew = map (\(off,row) -> (take (length row - size + 1) . drop off) row) . zip [0..size-1]
