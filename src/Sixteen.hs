module Sixteen where

run :: String
run = show . sum . map (read . pure) . show $ 2^1000
