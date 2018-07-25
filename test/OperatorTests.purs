module OperatorTests where

import Prelude

import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Data.String (trim)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

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
