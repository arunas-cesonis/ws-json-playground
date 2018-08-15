module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log, logShow)

import Data.Array
import Data.Traversable
import Data.Maybe (fromJust, maybe)
import Data.Either
import Data.Filterable (filterMap)
import Data.Set as S
import Data.Tuple
import Data.Map as M
import Data.Int (toNumber)
import Math as Math

import FRP.Event (Event, subscribe, fold, makeEvent)
import FRP.Event.Keyboard (getKeyboard, Keyboard)
import FRP.Event.Mouse (getMouse, Mouse)
import FRP.Event.AnimationFrame (animationFrame)
import FRP.Behavior.Keyboard (keys)
import FRP.Behavior.Mouse (position)
import FRP.Behavior (ABehavior, sample_)
import Vector (Vector(..), getX, getY, origin, scale, vec)

import Color (Color, black, white, rgba)
import Graphics.Drawing (Shape, render, lineWidth, path, outlined, outlineColor, Point, Drawing, fillColor, filled, rectangle, text)
import Graphics.Drawing.Font (font, serif)
import Graphics.Canvas (getCanvasElementById, getContext2D, getCanvasWidth, getCanvasHeight)

import Partial.Unsafe (unsafePartial)
import Effect.Ref as Ref

import Foreign as Foreign
import Foreign (F, Foreign)

import Network as Network
import Engine

import Simple.JSON as JSON
import Simple.JSON (class ReadForeign)
import Foreign.Object (Object)
import Foreign.Object as Object
import Shape
import Data.Generic.Rep as GR

red :: Color
red = rgba 255 0 0 1.0

green :: Color
green = rgba 0 255 0 1.0

type InputDevices =
  { keyboard :: Keyboard
  , mouse :: Mouse
  }

type InputState =
  { mousePosition :: Vector
  , keysDown :: S.Set String
  }

type Player =
  { position :: Vector
  , aimAngle :: Number
  }

type State =
  { stageSize :: Vector
  , debug :: String
  , player :: Player
  }

initialState :: Vector -> State
initialState stageSize =
  { stageSize
  , debug: ""
  , player:
    { position: scale 0.5 stageSize
    , aimAngle: 0.0
    }
  }

background :: Vector -> Drawing
background (Vector {x: w, y: h}) = filled (fillColor black) (rectangle 0.0 0.0 w h)

toPoint :: Vector -> Point
toPoint (Vector {x, y}) = {x, y}

centeredRectangle :: Number -> Number -> Number -> Number -> Shape
centeredRectangle x y w h = rectangle (x - w / 2.0) (y - h / 2.0) w h

avatar :: Player -> Drawing
avatar player@({position: Vector {x, y}}) = body <> gun
  where
    gunStart = toPoint player.position
    gunEnd = toPoint (player.position + (scale 15.0 $ vec (Math.sin player.aimAngle) (Math.cos player.aimAngle)))
    gun = outlined (lineWidth 5.0 <> outlineColor green) (path [gunStart, gunEnd])
    body = filled (fillColor red) (centeredRectangle x y 20.0 20.0)

draw :: State -> Drawing
draw state = background (state.stageSize)
          <> avatar state.player
          <> text (font serif 12 mempty) 20.0 20.0 (fillColor white) state.debug

update :: Action -> State -> Tuple State Command
update action state = Tuple state Noop

-- parser :: String -> Either Foreign.MultipleErrors Message
-- parser = JSON.readJSON

message :: Network.Socket -> Event String
message socket = filterMap identity (Network.messageEventToString <$> Network.message socket)

newtype W a = W a

instance showVector :: Show a => Show (W a) where
  show (W x) = show x

instance readMap :: ReadForeign a => ReadForeign (W (M.Map String a)) where
  readImpl = pure <<< W <<< objectToMap <=< JSON.read'
    where
      objectToMap :: forall a. Object a -> M.Map String a
      objectToMap x = M.fromFoldable ((Object.toUnfoldable x) :: Array (Tuple String a))

type XMap a =  W (M.Map String a)

type Message =
  { tag :: String
  , gameworld :: XMap Int
  }

parse :: String -> Either Foreign.MultipleErrors Message
parse = JSON.readJSON

main :: Effect Unit
main = do
  socket <- Network.connect "ws://127.0.0.1:8080"
  _ <- subscribe (message socket) \e->
    log e
  _ <- subscribe (Network.open socket) \_-> do
    log "connected"
    Network.send socket "123"
  let msg = parse "{\"hello\": \"world\", \"m\": {\"KEY\":123}}"
  logShow msg
  logShow (readShape "{\"tag\": \"Circle\", \"contents\": 100.0}")
  logShow (readShape "{\"tag\": \"Rectangle\", \"contents\": [11.0, 12.0]}")
  logShow (readShape "{\"tag\": \"Point\"}")
  pure unit
