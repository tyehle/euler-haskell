module Main where

import Seventeen

import System.CPUTime

main :: IO ()
main = do
  start <- getCPUTime
  let result = run
  end <- getCPUTime
  putStrLn result
  let diff = fromIntegral (end - start) / (10^12)
  putStrLn $ "Computation time: "++show diff++" sec"
