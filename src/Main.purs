module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Data.Maybe (fromJust)

import FRP.Event (Event, subscribe, fold)
import FRP.Event.Keyboard (down, getKeyboard)
import FRP.Event.AnimationFrame (animationFrame)
import FRP.Behavior.Keyboard (keys)
import FRP.Behavior (sample_)

import Graphics.Drawing (render)
import Color (black)
import Graphics.Drawing (Drawing, fillColor, filled, rectangle, text)
import Graphics.Canvas (getCanvasElementById, getContext2D)

import Partial.Unsafe (unsafePartial)

import Network (runNetwork)

draw _ = filled (fillColor black) (rectangle 0.0 0.0 20.0 20.0)

loop k x = show k

z kbd = fold loop (sample_ (keys kbd) animationFrame) "x"

main :: Effect Unit
main = do
  runNetwork
  mc <- getCanvasElementById "canvas"
  let canvas = unsafePartial (fromJust mc)
  ctx <- getContext2D canvas
  kbd <- getKeyboard
  _ <- subscribe (z kbd) (render ctx <<< draw)
  log "end of main"
