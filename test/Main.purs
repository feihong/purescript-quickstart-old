module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
-- import Control.Monad.Eff.Console (CONSOLE, log)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

-- main :: forall e. Eff (console :: CONSOLE | e) Unit
-- main = do
--   log "You should add some tests."

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  describe "Hello Test" do
    it "awesome" do
      (1 + 1) `shouldEqual` 2
