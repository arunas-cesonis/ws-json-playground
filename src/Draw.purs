module Draw
  ( drawWorld
  , getContext
  ) where

import Prelude
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Data.Maybe (fromJust)
import Data.Foldable (foldMap)
import Data.Map as M

import Color (rgba, black)
import Graphics.Drawing (Drawing, render, filled, circle, rectangle, fillColor, translate, rotate)
import Graphics.Canvas (getCanvasElementById, getContext2D, Context2D)

import Message

red = rgba 255 0 0 1.0
green = rgba 0 255 0 1.0

getContext :: String -> Effect Context2D
getContext id = do
  canvas <- unsafePartial fromJust <$> getCanvasElementById id
  getContext2D canvas

drawBackground :: Drawing
drawBackground = filled (fillColor black) (rectangle 0.0 0.0 1000.0 6000.0) 

drawObject :: Object -> Drawing
drawObject {center, shape, rotation} = case shape of
  MkRectangle w h -> rect w h
  MkCircle r -> t $ filled (fillColor green) (circle 0.0 1.0 r)
  MkSquare s -> rect s s
  where
    t = translate center.x center.y <<< rotate rotation.a
    rect w h = t $ filled (fillColor red) (rectangle 0.0 0.0 w h)

drawAvatar :: Avatar -> Drawing
drawAvatar {object} = drawObject object

drawAvatars :: WMap Avatar -> Drawing
drawAvatars (W m) = foldMap drawAvatar (M.values m)

drawObstacle :: Obstacle -> Drawing
drawObstacle {object} = drawObject object

drawObstacles :: WMap Obstacle -> Drawing
drawObstacles (W m) = foldMap drawObstacle (M.values m)

drawWorld :: GameWorld -> Drawing
drawWorld {avatars, obstacles} =
     drawBackground
  <> drawObstacles obstacles
  <> drawAvatars avatars
