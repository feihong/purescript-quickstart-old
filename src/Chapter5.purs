module Chapter5 where

import Prelude
import Data.Picture
import Data.Maybe (Maybe(..))

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

-- Skip binomial coefficients exercise

type Address = { street :: String, city :: String }

type Person = { name :: String, address :: Address }

livesInLA :: forall r1 r2.
  { address :: 
    { city :: String | r1 }
  | r2
  } -> Boolean
livesInLA { address: { city: "Los Angeles" } } = true
livesInLA _ = false

sameCity :: forall r1 r2.
 { address :: { city :: String | r1 } | r2 }
 -> { address :: { city :: String | r1 } | r2 }
 -> Boolean
sameCity {address: {city: c1}} {address: {city: c2}} = c1 == c2

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [x] = x
fromSingleton d _ = d

origin :: Point
origin = Point {x: 0.0, y: 0.0}

circle :: Shape
circle = Circle origin 10.0

expandAndCenter :: Shape -> Shape
expandAndCenter (Circle _ r) = Circle origin (r * 2.0)
expandAndCenter (Rectangle _ w h) = Rectangle origin (w * 2.0) (h * 2.0)
expandAndCenter x = x

extractText :: Shape -> Maybe String
extractText (Text _ text) = Just text
extractText _ = Nothing