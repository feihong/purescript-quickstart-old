module Chapter7Tests where

import Chapter7
import Prelude

import Control.Apply (lift2)
import Data.AddressBook (address)
import Data.Maybe (Maybe(..))
import Data.Validation.Semigroup (V, toEither)
import Test.Spec (Spec, describe, it)
import Data.Either (Either(..))
import Test.Spec.Assertions (shouldEqual)

getErrors :: forall a. V (Array String) a -> Array String
getErrors v = case toEither(v) of 
  Left errors -> errors
  Right _ -> []

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
