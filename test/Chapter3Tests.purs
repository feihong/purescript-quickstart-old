module Chapter3Tests where

import Chapter3
import Prelude

import Chapter3 (nameAppears, removeDuplicates)
import Data.Array (toUnfoldable)
import Data.List.Types ((:))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

addr1 = createAddress "123 Main St" "Chicago" "IL"
addr2 = createAddress "456 Xintiao Lu" "Shenyang" "LN"

entry1 = createEntry "Bob" "Robot" addr1
entry2 = createEntry "Tom" "Voltron" addr2

book1 = toUnfoldable [entry1, entry2]

book2 = entry1 : book1

spec :: Spec Unit
spec = 
  describe "Chapter 3" do
    it "nameAppears" do
      (nameAppears "bob" book1) `shouldEqual` true
      (nameAppears "Bob" book1) `shouldEqual` true
      (nameAppears "poo" book1) `shouldEqual` false
    
    it "removeDuplicates" do
      (removeDuplicates book2) `shouldEqual` book1
    