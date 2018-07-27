module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Data.Maybe (fromJust)

import FRP.Event (Event, subscribe, fold)
import FRP.Event.Keyboard (down, getKeyboard, Keyboard)
import FRP.Event.AnimationFrame (animationFrame)
import FRP.Behavior.Keyboard (keys)
import FRP.Behavior (sample_)

import Graphics.Drawing (render)
import Color (black, white, rgba)
import Graphics.Drawing (Drawing, fillColor, filled, rectangle, text)
import Graphics.Drawing.Font (font, serif, bold)
import Graphics.Canvas (getCanvasElementById, getContext2D, getCanvasWidth, getCanvasHeight)

import Partial.Unsafe (unsafePartial)

import Network (runNetwork)

red = rgba 255 0 0 1.0

type Vec2 =
  { x :: Number
  , y :: Number
  }

vec2 :: Number -> Number -> Vec2
vec2 x y = {x, y}

type State =
  { stageSize :: Vec2
  }

initialState :: Vec2 -> State
initialState stageSize =
  { stageSize
  }

background {x: w, y: h} = filled (fillColor black) (rectangle 0.0 0.0 w h)

draw state = background (state.stageSize)
          <> filled (fillColor red) (rectangle 20.0 20.0 20.0 20.0)
          <> text (font serif 12 bold) 20.0 20.0 (fillColor white) "HELLO"
 
loop k state = state
  where sk = show k

z :: Keyboard -> State -> Event State
z kbd state = fold loop (sample_ (keys kbd) animationFrame) state

main :: Effect Unit
main = do
  runNetwork
  mc <- getCanvasElementById "canvas"
  let canvas = unsafePartial (fromJust mc)
  ctx <- getContext2D canvas
  w <- getCanvasWidth canvas
  h <- getCanvasHeight canvas
  state <- pure $ initialState (vec2 w h)
  kbd <- getKeyboard
  _ <- subscribe (z kbd state) (render ctx <<< draw)
  log "end of main"
