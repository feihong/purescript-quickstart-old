-- https://github.com/paf31/purescript-book/blob/master/text/chapter2.md

module Chapter2 where

import Prelude

import Math (pi, sqrt)

diagonal :: Number -> Number -> Number
diagonal w h = sqrt $ w * w + h * h

circleArea :: Number -> Number
circleArea r = pi * r * r