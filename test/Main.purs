module Test.Main where

import Prelude
import Effect (Effect)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

import Euler (euler1)

main :: Effect Unit
main = run [consoleReporter] do
  describe "Hello Test" do
    it "addition" do
      (1 + 2) `shouldEqual` 3

  describe "Project Euler" do
    it "#1" do
      euler1 `shouldEqual` 233168