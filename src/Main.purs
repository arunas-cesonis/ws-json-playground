module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Data.Maybe (fromJust)

import FRP.Event (Event, subscribe, fold)
import FRP.Event.Keyboard (down, getKeyboard, Keyboard)
import FRP.Event.Mouse (getMouse, Mouse)
import FRP.Event.AnimationFrame (animationFrame)
import FRP.Behavior.Keyboard (keys)
import FRP.Behavior (sample_)

import Graphics.Drawing (render)
import Color (black, white, rgba)
import Graphics.Drawing (Drawing, fillColor, filled, rectangle, text)
import Graphics.Drawing.Font (font, serif)
import Graphics.Canvas (getCanvasElementById, getContext2D, getCanvasWidth, getCanvasHeight)

import Partial.Unsafe (unsafePartial)

import Network (runNetwork)

red = rgba 255 0 0 1.0

type InputDevices =
  { keyboard :: Keyboard
  , mouse :: Mouse
  }

type Vec2 =
  { x :: Number
  , y :: Number
  }

vec2 :: Number -> Number -> Vec2
vec2 x y = {x, y}

type State =
  { stageSize :: Vec2
  , debug :: String
  }

initialState :: Vec2 -> State
initialState stageSize =
  { stageSize
  , debug: ""
  }

background {x: w, y: h} = filled (fillColor black) (rectangle 0.0 0.0 w h)

avatar {x, y} = filled (fillColor red) (rectangle x y 20.0 20.0)

draw state = background (state.stageSize)
          <> avatar (vec2 5.0 5.0)
          <> text (font serif 12 mempty) 20.0 20.0 (fillColor white) state.debug
 
loop k state = state {debug = show k}

z :: InputDevices -> State -> Event State
z inputDevices state = fold loop (sample_ (keys inputDevices.keyboard) animationFrame) state

main :: Effect Unit
main = do
  runNetwork
  mc <- getCanvasElementById "canvas"
  let canvas = unsafePartial (fromJust mc)
  ctx <- getContext2D canvas
  w <- getCanvasWidth canvas
  h <- getCanvasHeight canvas
  state <- pure $ initialState (vec2 w h)
  keyboard <- getKeyboard
  mouse <- getMouse
  _ <- subscribe (z {keyboard, mouse} state) (render ctx <<< draw)
  log "end of main"
