module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log, logShow, errorShow)
import Data.Traversable (sequence)
import Data.Either
import FRP.Behavior (animate, Behavior, unfold)
import FRP.Event (Event, subscribe, makeEvent)
import Network as Network
import Message
import Graphics.Drawing (Drawing, render)
import Draw

messageEvent :: Network.Socket -> Event ActionResp
messageEvent socket = makeEvent \k->
  subscribe (Network.onStringMessage socket) (handler k)
  where
    handler k str = do
      case (readMessage str) of 
        Right s -> do
          logShow s
          void $ sequence (k <$> s)
        Left err -> void $ sequence (errorShow <$> err)

serverWorld :: Network.Socket -> Behavior GameWorld
serverWorld socket = unfold f (messageEvent socket) emptyGameWorld
  where
    f x z = case x of
      GameWorldResp world -> world
      MoveResp _ _ -> z
      RotateResp _ _ -> z

main :: Effect Unit
main = do
  socket <- Network.connect "ws://127.0.0.1:8080"
  context <- getContext "canvas"
  void $ animate (drawWorld <$> (serverWorld socket)) (render context)
