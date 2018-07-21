module Chapter5 where

import Prelude

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