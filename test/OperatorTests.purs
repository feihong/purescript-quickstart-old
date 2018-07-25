module OperatorTests where

import Prelude

import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (trim)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Control.Apply ((*>), applySecond)


spec :: Spec Unit
spec = 
  describe "Operator tests" do
    it "applyFlipped operator #" do
      let 
        result = 
          "   6     "
          # trim  
          # fromString
          # fromMaybe 0
          # (*) 2
          # (_ + 1)     -- operator section
               
      result `shouldEqual` 13
    it "Underscore in operator section" do
      let 
        arr = [{a: 1, b: "A"}, {a: 2, b: "B"}, {a: 3, b: "C"}]

      map _.a arr `shouldEqual` [1, 2, 3]
      map _.b arr `shouldEqual` ["A", "B", "C"]
    it "Sequencing operator *>" do
      -- *> is actually shorthand for the applySecond function:
      -- Combine two effectful actions, keeping only the result of the second
      -- applySecond :: forall a b f. Apply f => f a -> f b -> f b
      applySecond (Just 'a') (Just 2) `shouldEqual` Just 2
      applySecond (Just 'a') (Nothing :: Maybe Int) `shouldEqual` Nothing
      (Just 400 *> Just "foo") `shouldEqual` Just "foo"
      (Just 401 *> (Nothing :: Maybe String)) `shouldEqual` Nothing
