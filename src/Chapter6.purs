module Chapter6 where

import Prelude

newtype Complex = Complex
  { real :: Number
  , imaginary :: Number
  }

instance showComplex :: Show Complex where
  show (Complex {real, imaginary}) = 
    "Complex " <> show real <> " + " <> show imaginary <> "i"

instance eqComplex :: Eq Complex where
  eq (Complex {real: r1, imaginary: i1}) (Complex {real: r2, imaginary: i2}) =
    r1 == r2 && i1 == i2

data NonEmpty a = 
  NonEmpty a (Array a)

instance eqNonEmpty :: Eq a => Eq (NonEmpty a) where
  eq (NonEmpty v1 a1) (NonEmpty v2 a2) = v1 == v2 && a1 == a2

instance semigroupNonEmpty :: Semigroup (NonEmpty a) where
  append (NonEmpty v1 a1) (NonEmpty v2 a2) = NonEmpty v1 (a1 <> [v2] <> a2)

-- Note that you don't need the type parameter for NonEmpty here
instance functorNonEmpty :: Functor NonEmpty where
  map f (NonEmpty v arr) = NonEmpty (f v) (map f arr)

data Extended a 
  = Finite a 
  | Infinite

instance eqExtended :: Eq a => Eq (Extended a) where
  eq (Finite a) (Finite b) = a == b
  eq Infinite Infinite = true
  eq _ _ = false

instance ordExtended :: Ord a => Ord (Extended a) where
  compare (Finite _) Infinite = LT
  compare Infinite (Finite _) = GT
  compare Infinite Infinite = EQ
  compare (Finite x) (Finite y) = compare x y
