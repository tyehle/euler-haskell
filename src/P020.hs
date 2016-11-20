module P020 where

run :: IO ()
run = print . sum $ digits
  where
    big = product [2..100]
    digits = map (read . pure) . show $ big
