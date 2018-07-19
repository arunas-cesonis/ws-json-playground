module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Network (runNetwork)

import FRP.Event (Event, subscribe, fold)
import FRP.Event.Keyboard (down, getKeyboard)
import FRP.Event.AnimationFrame (animationFrame)
import FRP.Behavior.Keyboard (keys)
import FRP.Behavior (sample_)

loop _ x = x

z kbd = fold loop (sample_ (keys kbd) animationFrame) "x"

main :: Effect Unit
main = do
  runNetwork
  kbd <- getKeyboard
  _ <- subscribe (z kbd) (\x-> log x)
  log "end of main"
