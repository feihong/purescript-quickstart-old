module Chapter7Tests where

import Chapter6
import Prelude

import Control.Apply (lift2)
import Data.Maybe (Maybe(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Chapter 7" do
  it "lift2" do
    lift2 (+) (Just 2) Nothing `shouldEqual` Nothing
    lift2 (*) (Just 4) (Just 2) `shouldEqual` Just 8
    lift2 (-) Nothing (Just 5) `shouldEqual` Nothing
    lift2 mod (Just 33) (Just 10) `shouldEqual` Just 3