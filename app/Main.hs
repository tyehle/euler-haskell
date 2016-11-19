module Main where

import P067

import System.CPUTime

main :: IO ()
main = do
  start <- getCPUTime
  run
  end <- getCPUTime
  let diff = fromIntegral (end - start) / (10^12)
  putStrLn $ "Computation time: "++show diff++" sec"
