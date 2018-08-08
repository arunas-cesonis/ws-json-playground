module Network(runNetwork) where

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
import Web.Event.Event (Event)

import FRP.Event as FRPE

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.?))
import Data.Argonaut (jsonEmptyObject)

import Partial.Unsafe (unsafePartial)
import Effect.Ref as Ref

import Foreign (readString, unsafeToForeign)

newtype Message = Message
  { text :: String
  }

instance encodeJsonMessage :: EncodeJson Message where
  encodeJson (Message o) =
    "text" := o.text ~> jsonEmptyObject

instance decodeJsonMessage :: DecodeJson Message where
  decodeJson json = do
    x <- decodeJson json
    text <- x .? "text"
    pure $ Message {text}

z :: String
z = stringify $ encodeJson (Message {text})
  where
    text = "hello"

eventToString :: Event -> String
eventToString ev =
  case ME.fromEvent ev of
    Just msgEvent -> (fromMaybe "" <<< hush <<< runExcept <<< readString <<< unsafeToForeign <<< ME.data_) msgEvent
    Nothing -> ""

dec :: String -> Either String Message
dec str = decodeJson =<< jsonParser str

eventToMessage :: Event -> String
eventToMessage ev =
  case (dec (eventToString ev)) of
    Right (Message {text}) -> text
    Left err -> err

message :: WS.WebSocket -> FRPE.Event Event
message socket = FRPE.makeEvent \k-> do
  let target = (WS.toEventTarget socket)
  listener <- EET.eventListener k
  EET.addEventListener WSET.onMessage listener false target
  pure (EET.removeEventListener WSET.onMessage listener false target)

runNetwork :: Effect Unit
runNetwork = do
  socket <- WS.create "ws://localhost:7080" []
  log "end of Network.run"
