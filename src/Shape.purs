module Shape(Shape(..)) where

import Prelude (class Show)
import Data.Generic.Rep as GR
import Data.Generic.Rep.Show (genericShow)

data Shape =
    Square Number
  | Rectangle Number Number
  | Circle Number

derive instance genericShape :: GR.Generic Shape _

instance showShape :: Show Shape where
  show = genericShow
