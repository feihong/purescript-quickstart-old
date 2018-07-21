module OperatorTests where

import Prelude

import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Data.String (trim)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

result :: Int
result = 
  "   6     "
  # trim  
  # fromString
  # fromMaybe 0
  # (*) 2
  # (_ + 1)     -- operator section

spec :: Spec Unit
spec = 
  describe "Operator tests" do
    it "applyFlipped operator #" do
      result `shouldEqual` 13
