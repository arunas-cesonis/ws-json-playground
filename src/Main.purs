module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log, logShow, errorShow)
import Data.Traversable (sequence)
import Data.Either
import Data.Maybe
import Graphics.Drawing (Drawing, render)
import Graphics.Canvas (getContext2D, getCanvasElementById, Context2D)
import FRP.Behavior (animate, Behavior, unfold)
import FRP.Event (Event, subscribe, makeEvent)
import Network as Network
import Partial.Unsafe (unsafePartial)
import Message

messageEvent :: Network.Socket -> Event ActionResp
messageEvent socket = makeEvent \k->
  subscribe (Network.onStringMessage socket) (handler k)
  where
    handler k str = do
      case (readMessage str) of 
        Right s -> void $ sequence (k <$> s)
        Left err -> void $ sequence (errorShow <$> err)

serverWorld :: Network.Socket -> Behavior GameWorld
serverWorld socket = unfold f (messageEvent socket) emptyGameWorld
  where
    f x z = case x of
      GameWorldResp world -> world
      MoveResp _ _ -> z
      RotateResp _ _ -> z

drawWorld :: GameWorld -> Drawing
drawWorld world = mempty

getContext :: Effect Context2D
getContext = do
  canvas <- unsafePartial fromJust <$> getCanvasElementById "canvas"
  getContext2D canvas

main :: Effect Unit
main = do
  socket <- Network.connect "ws://127.0.0.1:8080"
  context <- getContext
  void $ animate (drawWorld <$> (serverWorld socket)) (render context)
