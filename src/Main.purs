module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Euler (euler1)

main :: Effect Unit
main = do
  log "Hello sailor!"

  log $ "Answer is " <> show euler1
