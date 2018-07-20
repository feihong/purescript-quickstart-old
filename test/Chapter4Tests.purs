module Chapter4Tests where

import Prelude

import Chapter4
import Data.Array((..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = 
  describe "Chapter 4" do
    it "isPrime" do
      isPrime 3 `shouldEqual` true
      isPrime 11 `shouldEqual` true
      isPrime 20 `shouldEqual` false
      isPrime 56 `shouldEqual` false

    it "cartesianProduct" do
      cartesianProduct (1 .. 3) (10 .. 13) `shouldEqual` 
        [[1,10],[1,11],[1,12],[1,13],[2,10],[2,11],[2,12],[2,13],[3,10],[3,11],[3,12],[3,13]]

    it "triples" do 
      triples 4 `shouldEqual` []
      triples 10 `shouldEqual` [[3,4,5]]
      triples 15 `shouldEqual` [[3,4,5],[5,12,13],[6,8,10]]