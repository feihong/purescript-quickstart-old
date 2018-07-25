module Chapter7Tests where

import Chapter7
import Prelude

import Control.Apply (lift2)
import Data.AddressBook (address)
import Data.Foldable (foldl, foldr, foldMap)
import Data.Array (snoc)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Validation.Semigroup (V, toEither)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

getErrors :: forall a. V (Array String) a -> Array String
getErrors v = case toEither(v) of 
  Left errors -> errors
  Right _ -> []

empty :: Array Int
empty = []

tree1 = Branch Leaf 4 Leaf
tree2 = Branch (Branch Leaf 3 Leaf) 4 (Branch Leaf 5 Leaf)
tree3 = Branch 
    (Branch (Branch Leaf 1 Leaf) 2 (Branch Leaf 3 Leaf)) 
    4 
    (Branch Leaf 5 Leaf)

spec :: Spec Unit
spec = describe "Chapter 7" do
  it "lift2" do
    lift2 (+) (Just 2) Nothing `shouldEqual` Nothing
    lift2 (*) (Just 4) (Just 2) `shouldEqual` Just 8
    lift2 (-) Nothing (Just 5) `shouldEqual` Nothing
    lift2 mod (Just 33) (Just 10) `shouldEqual` Just 3
  it "Validate state is 2 alphabetic characters" do
    (address "123 Main St" "Chicago" "Illinois" # validateAddress # getErrors)
      `shouldEqual` 
      ["Field state must be two alphabetic characters"]
    (address "123 Main St" "Chicago" "IL" # validateAddress # getErrors)
      `shouldEqual` 
      []
  it "Validate street and city are not whitespace" do
    (address "    " "" "IL" # validateAddress # getErrors)
      `shouldEqual` 
      ["Field Street cannot be whitespace", "Field City cannot be empty"]
  it "Tree foldl" do
    foldl snoc empty Leaf `shouldEqual` []
    foldl snoc empty tree1 `shouldEqual` [4]
    foldl snoc empty tree2 `shouldEqual` [3,4,5]
    foldl snoc empty tree3 `shouldEqual` [1,2,3,4,5]
  it "Tree foldr" do
    foldr (flip snoc) empty Leaf `shouldEqual` []
    foldr (flip snoc) empty tree1 `shouldEqual` [4]
    foldr (flip snoc) empty tree2 `shouldEqual` [5,4,3]
    foldr (flip snoc) empty tree3 `shouldEqual` [5,4,3,2,1]
  it "Tree foldMap" do
    foldMap (\v -> [v]) tree1 `shouldEqual` [4]
    foldMap (\v -> [v]) tree2 `shouldEqual` [3,4,5]
    foldMap (\v -> [v]) tree3 `shouldEqual` [1,2,3,4,5]
