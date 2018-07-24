module Chapter6Tests where

import Chapter6
import Prelude

import Data.Foldable (foldl, foldr, foldMap)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

omList :: OneMore Array Int
omList = OneMore 1 [2,3,4]

spec :: Spec Unit
spec = 
  describe "Chapter 6" do
    it "OneMore foldl" do
      foldl (\acc -> append acc <<< show) "hey" omList `shouldEqual` "hey1234"
    it "OneMore foldr" do
      foldr (\v acc -> acc <> show v) "hey" omList `shouldEqual` "hey4321"
    it "OneMore foldMap" do
      foldMap show omList `shouldEqual` "1234"