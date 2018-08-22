module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log, logShow, errorShow)
import Data.Traversable (sequence)
import Data.Either
import FRP.Behavior (animate, Behavior)
import FRP.Event (Event, subscribe, makeEvent)
import Network as Network
import Message

messageEvent :: Network.Socket -> Event ActionResp
messageEvent socket = makeEvent \k->
  subscribe (Network.onStringMessage socket) (handler k)
  where
    handler k str =
      case (readMessage str) of 
        Right s -> void $ sequence (k <$> s)
        Left err -> void $ sequence (errorShow <$> err)

main :: Effect Unit
main = do
  socket <- Network.connect "ws://127.0.0.1:8080"
  void $ subscribe (messageEvent socket) logShow
  void $ animate (pure 1) logShow 
