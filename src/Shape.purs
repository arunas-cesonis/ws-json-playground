module Shape(Shape(..)) where

import Prelude (class Show, (<$>))
import Control.Alt ((<|>))
import Data.Generic.Rep as GR
import Data.Generic.Rep.Show (genericShow)

import Simple.JSON as JSON

import Foreign as Foreign
import Foreign (Foreign)

data Shape =
    Square Number
  | Circle Number
  | Rectangle Number Number


ok = GR.Constructor "Square"

derive instance genericShape :: GR.Generic Shape _

instance showShape :: Show Shape where
  show = genericShow
