module Test.Main where

import Prelude

import Chapter2 (diagonal)
import Data.Ord (abs)
import Effect (Effect)
import Euler (euler1)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

main :: Effect Unit
main = run [consoleReporter] do
  describe "Arithmetic" do
    it "addition" do
      (1 + 2) `shouldEqual` 3
    it "mod" do
      (77 `mod` 10) `shouldEqual` 7

  describe "Project Euler" do
    it "#1" do
      euler1 `shouldEqual` 233168

  describe "Chapter2" do
    it "diagonal" 
      let 
        result = diagonal 4.0 5.0
        expected = 6.4
        diff = abs $ result - expected
      in do
        (diff < 0.01) `shouldEqual` true