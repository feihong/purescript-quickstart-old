module Chapter6 where

import Prelude

import Data.Array ((:), null)
import Data.Array.Partial (head, tail)
import Data.Foldable (class Foldable, foldMap, foldl, foldr, maximum)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)

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

instance foldableNonEmpty :: Foldable NonEmpty where
  foldl f b (NonEmpty x xs) = foldl f b (x : xs)
  foldr f b (NonEmpty x xs) = foldr f b (x : xs)
  foldMap f (NonEmpty x xs) = foldMap f (x : xs)

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

-- Given a type constructor f which defines an ordered container (and so has a
-- Foldable instance), we can create a new container type which includes an 
-- extra element at the front
data OneMore f a = OneMore a (f a)

instance foldableOneMore :: Foldable f => Foldable (OneMore f) where
  foldl f b (OneMore x fx) = foldl f (f b x) fx
  foldr f b (OneMore x fx) = f x $ foldr f b fx
  foldMap f (OneMore x fx) = f x <> foldMap f fx

unsafeMaxArray :: Partial => Array Int -> Int
unsafeMaxArray = fromJust <<< maximum

{-
There are two laws for the Action type class: 

  act mempty a = a
  act (m1 <> m2) a = act m1 (act m2 a)

That is, the action respects the operations defined by the Monoid class. 
-}

class Monoid m <= Action m a where
  act :: m -> a -> a

newtype Multiply = Multiply Int

instance semigroupMultiply :: Semigroup Multiply where
  append (Multiply n) (Multiply m) = Multiply (n * m)

instance monoidMultiply :: Monoid Multiply where
  mempty = Multiply 1

instance repeatAction :: Action Multiply String where
  act (Multiply 1) s = s
  act (Multiply n) s | n <= 0 = ""
  act (Multiply n) s = s <> act (Multiply $ n - 1) s

-- Write an instance Action m a => Action m (Array a), where the action on
-- arrays is defined by acting on each array element independently
instance arrayAction :: Action m a => Action m (Array a) where
  act _ [] = []
  act m arr =
    act m (unsafePartial head arr) : act m (unsafePartial tail arr)

newtype Self m = Self m

-- Write an instance for Action m (Self m), where the monoid m acts on itself 
-- using append
instance selfAction :: Action m m => Action m (Self m) where
  act m (Self a) = Self (act m a)