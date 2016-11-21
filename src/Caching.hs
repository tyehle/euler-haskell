module Caching where

import Data.Array

memoize :: Ix a => (a -> b) -> (a, a) -> a -> b
memoize f bounds = (!) cache
  where
    cache = listArray bounds [f i | i <- range bounds]
