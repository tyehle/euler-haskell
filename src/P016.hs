module P016 where

run :: IO ()
run = print . sum . map (read . pure) . show $ 2^1000
