module Euler where

import Prelude
import Data.List (range, filter)
import Data.Foldable (sum)

euler1 :: Int
euler1 =
  sum 
  $ filter (\n -> mod n 3 == 0 || mod n 5 == 0) 
  $ range 0 999
