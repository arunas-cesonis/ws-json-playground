module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Network (runNetwork)

main :: Effect Unit
main = do
  runNetwork
  log "end of main"
