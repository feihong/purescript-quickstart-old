module Test.Main where

import Prelude

import Chapter2 (circleArea, diagonal)
import Chapter3Tests as Ch3
import Chapter4Tests as Ch4
import OperatorTests as OperatorTests
import Data.Ord (abs)
import Effect (Effect)
import Effect.Aff (Aff)
import Euler (euler1)
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)


diffLessThan :: forall t. Ord t => Ring t => Show t => t -> t -> t -> Aff Unit
diffLessThan v1 v2 max = 
  let 
    diff = abs $ v1 - v2
  in
    when (diff > max) $
      fail $ "The difference between " <> show v1 <> " and " <> show v2 <>
             " is greater than " <> show max

main :: Effect Unit
main = run [consoleReporter] do
  describe "Miscellaneous" do
    it "mod" do
      (77 `mod` 10) `shouldEqual` 7    
    it "Project Euler #1" do
      euler1 `shouldEqual` 233168

  describe "Chapter2" do
    it "diagonal" do
      diffLessThan (diagonal 4.0 5.0) 6.403 0.001
    it "circleArea" do
      diffLessThan (circleArea 3.5) 38.485 0.001

  Ch3.spec
  Ch4.spec
  OperatorTests.spec
