module Chapter7Tests where

import Chapter7
import Prelude

import Control.Apply (lift2, applySecond)
import Data.AddressBook (PhoneType(..), address, phoneNumber, person)
import Data.AddressBook.Validation (validatePhoneNumber)
import Data.Array (snoc)
import Data.Either (Either(..))
import Data.Foldable (foldl, foldr, foldMap)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence, traverse)
import Data.Validation.Semigroup (V, toEither)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)


getErrors :: forall a. V (Array String) a -> Array String
getErrors v = case toEither(v) of 
  Left errors -> errors
  Right _ -> []

empty :: Array Int
empty = []

tree1 :: Tree Int
tree1 = Branch Leaf 4 Leaf
tree2 :: Tree Int
tree2 = Branch (Branch Leaf 3 Leaf) 4 (Branch Leaf 5 Leaf)
tree3 :: Tree Int
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

  it "Sequencing operator *>" do
      -- *> is actually shorthand for the applySecond function      
      applySecond (Just 'a') (Just 2) `shouldEqual` Just 2
      applySecond (Just 'a') (Nothing :: Maybe Int) `shouldEqual` Nothing
      (Just 400 *> Just "foo") `shouldEqual` Just "foo"
      (Just 401 *> (Nothing :: Maybe String)) `shouldEqual` Nothing
      ((Right 42 :: Either String Int) *> pure 45) 
        `shouldEqual` pure 45
      ((Left "whoa now" :: Either String Int) *> pure 45)
        `shouldEqual` Left "whoa now"

  it "nonWhitespace" do
    nonWhitespace "Street" "111 Pure Ave" `shouldEqual` pure (unit)
    (nonWhitespace "Street" "   " # toEither) 
      `shouldEqual` Left ["Field Street cannot be whitespace"]

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

  it "traverse" do
    traverse (\v -> Just $ v * 2) [1, 2, 3] `shouldEqual` Just [2,4,6]
    traverse (\v -> if v == 2 then Nothing else Just v) [1,2,3] 
      `shouldEqual` Nothing

  it "traverse validatePhoneNumber"  do
    let numbers = [
          phoneNumber HomePhone "666-111-2222",
          phoneNumber WorkPhone "222-555-8888"
        ]
        err = "Field 'Number' did not match the required format"
    traverse validatePhoneNumber numbers 
      `shouldEqual` pure numbers
    (traverse validatePhoneNumber [
        phoneNumber HomePhone "444-555-6666", 
        phoneNumber WorkPhone "111-222-333"
      ] # toEither) `shouldEqual` Left [err]
    (traverse validatePhoneNumber [
        phoneNumber HomePhone "444-5555-6666", 
        phoneNumber WorkPhone "111-222-333"
      ] # toEither) `shouldEqual` Left [err, err]

  it "sequence" do
    sequence (Just [1,2,3]) `shouldEqual` [Just 1, Just 2, Just 3]
    sequence (Nothing :: Maybe (Array Int)) `shouldEqual` [Nothing]
    sequence [Just 1, Just 2, Just 3] `shouldEqual` Just [1,2,3]
    sequence [Just 1, Nothing, Just 3] `shouldEqual` Nothing

  it "Tree foldl" do
    foldl snoc empty Leaf `shouldEqual` []
    foldl snoc empty tree1 `shouldEqual` [4]
    foldl snoc empty tree2 `shouldEqual` [3,4,5]
    foldl snoc empty tree3 `shouldEqual` [1,2,3,4,5]

  it "Tree foldr" do
    let fsnoc = flip snoc
    foldr fsnoc empty Leaf `shouldEqual` []
    foldr fsnoc empty tree1 `shouldEqual` [4]
    foldr fsnoc empty tree2 `shouldEqual` [5,4,3]
    foldr fsnoc empty tree3 `shouldEqual` [5,4,3,2,1]

  it "Tree foldMap" do
    foldMap (\v -> [v]) tree1 `shouldEqual` [4]
    foldMap (\v -> [v]) tree2 `shouldEqual` [3,4,5]
    foldMap (\v -> [v]) tree3 `shouldEqual` [1,2,3,4,5]

  it "Tree traverse" do
    traverse (\v -> Just $ v + 1) tree1 `shouldEqual` Just (Branch Leaf 5 Leaf)
    traverse (\v -> Just $ v + 1) Leaf `shouldEqual` Just Leaf
    traverse (\v -> if v == 4 then Nothing else Just $ v + 1) tree2 
      `shouldEqual` Nothing

  it "Tree sequence" do
    sequence (Nothing :: Maybe (Tree Int)) `shouldEqual` Branch Leaf Nothing Leaf
    sequence (Just tree1) `shouldEqual` (Branch Leaf (Just 4) Leaf)

  it "validatePerson" do
    let p1 = person "John" "Smith"
         (Just $ address "123 Fake St." "FakeTown" "CA")
         [ phoneNumber HomePhone "555-555-5555"
         , phoneNumber CellPhone "555-555-0000"
         ]
        p2 = person "John" "Smith" 
         Nothing
         [ phoneNumber HomePhone "555-555-5555"
         , phoneNumber CellPhone "555-555-0000"
         ]
        p3 = person "John" "Smith"
         (Just $ address "" " " "C1")
         [ phoneNumber HomePhone "555-555-555"
         , phoneNumber CellPhone "555-555-0000"
         ]
    (validatePerson p1 # toEither) `shouldEqual` Right p1
    (validatePerson p2 # toEither) `shouldEqual` Right p2
    (validatePerson p3 # toEither) 
      `shouldEqual` Left [
        "Field Street cannot be empty",
        "Field City cannot be whitespace",
        "Field state must be two alphabetic characters",
        "Field 'Number' did not match the required format"
      ]
