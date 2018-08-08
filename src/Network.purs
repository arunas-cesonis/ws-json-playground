module Network(connect, open, message, send, messageEventToString, Socket) where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..), fromMaybe, fromJust, maybe)

import Control.Monad.Except (runExcept)

import Web.Socket.WebSocket as WS
import Web.Socket.Event.EventTypes as WSET
import Web.Socket.Event.MessageEvent as ME
import Web.Event.EventTarget as EET
import Web.Event.Event (Event, EventType(..))

import FRP.Event as FRPE

import Partial.Unsafe (unsafePartial)
import Effect.Ref as Ref

import Foreign (readString, unsafeToForeign)

newtype Message = Message
  { text :: String
  }

messageEventToString :: Event -> Maybe String
messageEventToString ev =
  case ME.fromEvent ev of
    Just msgEvent -> (hush <<< runExcept <<< readString <<< unsafeToForeign <<< ME.data_) msgEvent
    Nothing -> Nothing

type Socket = WS.WebSocket

makeWSEvent :: EventType -> Socket -> FRPE.Event Event
makeWSEvent eventType socket = FRPE.makeEvent \k-> do
  let target = (WS.toEventTarget socket)
  listener <- EET.eventListener k
  EET.addEventListener eventType listener false target
  pure (EET.removeEventListener eventType listener false target)

message :: Socket -> FRPE.Event Event
message = makeWSEvent WSET.onMessage

open :: Socket -> FRPE.Event Event
open = makeWSEvent WSET.onOpen

connect :: String -> Effect Socket
connect url = WS.create url []

send :: Socket -> String -> Effect Unit
send = WS.sendString
