module Shape(Shape(..), readShape) where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Except (throwError)
import Data.Generic.Rep as GR
import Data.Either
import Data.Generic.Rep.Show (genericShow)
import Type.Prelude (class IsSymbol, SProxy(..), reflectSymbol)
import Foreign.Index (readIndex, readProp)

import Simple.JSON as JSON

import Foreign as Foreign
import Foreign (Foreign, readString)
import GenericJSON (enumReadForeign)

data Shape =
    Square Number
  | Point
  | Circle Number
  | Rectangle Number Number
  | NamedCircle String Number
  | NamedRectangle String Number Number

derive instance genericShape :: GR.Generic Shape _

instance showShape :: Show Shape where
  show = genericShow

instance shapeReadForeign :: JSON.ReadForeign Shape where
  readImpl = enumReadForeign

readShape :: String -> Either Foreign.MultipleErrors Shape
readShape = JSON.readJSON
