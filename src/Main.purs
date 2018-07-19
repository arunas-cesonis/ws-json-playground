module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Data.Foldable (for_)
import Data.Either (Either(..), either, hush)
import Data.Maybe (Maybe(..), fromMaybe)

import Control.Monad.Except (runExcept)

import Web.Socket.WebSocket as WS
import Web.Socket.Event.EventTypes as WSET
import Web.Socket.Event.MessageEvent as ME
import Web.Event.EventTarget as EET

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.?))
import Data.Argonaut (jsonEmptyObject)

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

z = stringify $ encodeJson (Message {text})
  where
    text = "hello"

eventToString ev =
  case ME.fromEvent ev of
    Just msgEvent -> (fromMaybe "" <<< hush <<< runExcept <<< readString <<< unsafeToForeign <<< ME.data_) msgEvent
    Nothing -> ""

dec :: String -> Either String Message
dec str = decodeJson =<< jsonParser str

eventToMessage ev =
  case (dec (eventToString ev)) of
    Right (Message {text}) -> text
    Left err -> err

main :: Effect Unit
main = do
  socket <- WS.create "ws://localhost:7080" []
  listener <- EET.eventListener \ev -> do
    log (eventToMessage ev)

  openListener <- EET.eventListener \ev -> do
    log "open"
    WS.sendString socket z
  EET.addEventListener WSET.onMessage listener false (WS.toEventTarget socket)
  EET.addEventListener WSET.onOpen openListener false (WS.toEventTarget socket)
  log "end of main"
