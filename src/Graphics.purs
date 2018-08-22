module Graphics
  ( drawWorld
  , getContext
  ) where

import Prelude
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Data.Maybe (fromJust)

import Graphics.Drawing (Drawing, render)
import Graphics.Canvas (getCanvasElementById, getContext2D, Context2D)
import Graphics

import Message

getContext :: String -> Effect Context2D
getContext id = do
  canvas <- unsafePartial fromJust <$> getCanvasElementById id
  getContext2D canvas

drawWorld :: GameWorld -> Drawing
drawWorld {avatars, obstacles} = mempty
