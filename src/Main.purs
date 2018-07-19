module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Web.Socket.WebSocket as WS
import Web.Socket.Event.EventTypes as WSET
import Web.Event.EventTarget as EET

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Argonaut (jsonEmptyObject)

newtype Message = Message
  { text :: String
  }

instance encodeJsonMessage :: EncodeJson Message where
  encodeJson (Message o) =
    "text" := o.text ~> jsonEmptyObject

z = stringify $ encodeJson (Message {text})
  where
    text = "hello"

main :: Effect Unit
main = do
  socket <- WS.create "ws://localhost:7080" []
  listener <- EET.eventListener \ev -> do
    log "123"
  openListener <- EET.eventListener \ev -> do
    log "open"
    WS.sendString socket z
  EET.addEventListener WSET.onMessage listener false (WS.toEventTarget socket)
  EET.addEventListener WSET.onOpen openListener false (WS.toEventTarget socket)
  log "end of main"
