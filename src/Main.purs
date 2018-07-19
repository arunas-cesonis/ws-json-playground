module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Web.Socket.WebSocket as WS
import Web.Socket.Event.EventTypes as WSET
import Web.Event.EventTarget as EET

-- conn <- WS.create "ws://echo.websocket.org" []

main :: Effect Unit
main = do
  socket <- WS.create "ws://localhost:7080" []
  listener <- EET.eventListener \ev -> do
    log "123"
  openListener <- EET.eventListener \ev -> do
    log "open"
    WS.sendString socket "HELLO"
  EET.addEventListener WSET.onMessage listener false (WS.toEventTarget socket)
  EET.addEventListener WSET.onOpen openListener false (WS.toEventTarget socket)
  log "end of main"
