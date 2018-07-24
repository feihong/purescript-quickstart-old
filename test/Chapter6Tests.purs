module Chapter6Tests where

import Chapter6
import Prelude

import Data.Either (Either(..))
import Data.Foldable (foldl, foldr, foldMap)
import Data.Hashable (hashCode, hash)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
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
    it "hash char" do
      hash 44 `shouldEqual` hash 44
      hash 'c' `shouldEqual` hashCode 99
      hash "foo" `shouldEqual` hash 25857
      hash [1,2,3] `shouldEqual` hash 17238
      hash (Just "bar") `shouldEqual` hash 44290
      hash (Nothing :: Maybe Int) `shouldEqual` hash 0
      hash (Tuple 6 'p') `shouldEqual` hash 6150
      hash (Right "wow" :: Either Int String) `shouldEqual` hash 62956
    