module Network
  (connect,
    onOpen,
    onMessage, onStringMessage, send, messageEventToString, close, onClose, Socket) where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Data.Either (Either(..), hush)
import Data.Filterable (filterMap)
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

onMessage :: Socket -> FRPE.Event Event
onMessage = makeWSEvent WSET.onMessage

onOpen :: Socket -> FRPE.Event Event
onOpen = makeWSEvent WSET.onOpen

onClose :: Socket -> FRPE.Event Event
onClose = makeWSEvent WSET.onClose

onStringMessage :: Socket -> FRPE.Event String
onStringMessage socket = filterMap identity (messageEventToString <$> onMessage socket)

connect :: String -> Effect Socket
connect url = WS.create url []

send :: Socket -> String -> Effect Unit
send = WS.sendString

close :: Socket -> Effect Unit
close = WS.close
