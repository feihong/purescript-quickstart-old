module Chapter7 where

import Data.AddressBook
import Prelude

import Data.Either (Either(..))
import Data.Foldable (foldMap, foldl, foldr)
import Data.Maybe (Maybe(..))
import Data.String.Regex (Regex, test, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (class Foldable, class Traversable, traverse, sequence)
import Data.Validation.Semigroup (V, invalid)
import Partial.Unsafe (unsafePartial)

combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe Nothing = pure Nothing
combineMaybe (Just fx) = Just <$> fx

type Errors = Array String

matches :: String -> Regex -> String -> V Errors Unit
matches _ regex value | test regex value = pure unit
matches field _ _ = invalid ["Field '" <> field <> 
                    "' did not match the required format"]

whitespaceRegex :: Regex
whitespaceRegex =
  unsafePartial
    case regex "^[\\s]+$" noFlags of
      Right r -> r

nonWhitespace :: String -> String -> V Errors Unit
nonWhitespace field "" = invalid ["Field " <> field <> " cannot be empty"]
nonWhitespace field value | test whitespaceRegex value = 
  invalid ["Field " <> field <> " cannot be whitespace"]
nonWhitespace _ _ = pure unit

stateRegex :: Regex
stateRegex =
  unsafePartial
    case regex "^[a-zA-Z]{2}$" noFlags of
      Right r -> r

validateState :: String -> V Errors Unit
validateState text =
  case test stateRegex text of
    true -> pure unit
    false -> invalid ["Field state must be two alphabetic characters"]

validateAddress :: Address -> V Errors Address
validateAddress (Address o) =
  address <$> (nonWhitespace "Street" o.street *> pure o.street)
          <*> (nonWhitespace "City"   o.city   *> pure o.city)
          <*> (validateState o.state *> pure o.state)

data Tree a = Leaf | Branch (Tree a) a (Tree a)

instance showTree :: Show a => Show (Tree a) where
  show Leaf = "Leaf"
  show (Branch l x r) = 
    "(Branch " <> show l <> " " <> show x <> " " <> show r <> ")"

derive instance eqTree :: Eq a => Eq (Tree a)

instance functorTree :: Functor Tree where
  map f Leaf = Leaf
  map f (Branch l x r) = Branch (f <$> l) (f x) (f <$> r)

instance applyTree :: Apply Tree where
  apply (Branch _ f _) t@(Branch _ _ _) = f <$> t
  apply _ _ = Leaf

instance applicativeTree :: Applicative Tree where
  pure x = Branch Leaf x Leaf

instance foldableTree :: Foldable Tree where
  foldl f b Leaf = b
  foldl f b (Branch l x r) = foldl f cv r
    where lv = foldl f b l
          cv = f lv x

  foldr f b Leaf = b
  foldr f b (Branch l x r) = foldr f cv l
    where rv = foldr f b r
          cv = f x rv

  foldMap f t = foldl (\a -> append a <<< f) mempty t

instance traversableTree :: Traversable Tree where
  traverse f Leaf = pure Leaf
  traverse f (Branch l x r) = 
    Branch <$> traverse f l <*> f x <*> traverse f r

  sequence = traverse identity
