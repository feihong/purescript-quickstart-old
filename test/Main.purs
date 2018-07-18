module Test.Main where

import Prelude

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