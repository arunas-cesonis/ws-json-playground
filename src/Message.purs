module Message where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Except (throwError)
import Data.Generic.Rep as GR
import Data.Either
import Data.Map as M
import Data.Tuple
import Data.Generic.Rep.Show (genericShow)
import Type.Prelude (class IsSymbol, SProxy(..), reflectSymbol)
import Foreign.Index (readIndex, readProp)
import Foreign.Object as FObject

import Simple.JSON as JSON
import GenericJSON (enumReadForeign)

import Foreign as Foreign
import Foreign (Foreign, readString)

data Shape =
    MkSquare Number
  | MkCircle Number
  | MkRectangle Number Number

derive instance genericShape :: GR.Generic Shape _

instance showShape :: Show Shape where
  show = genericShow

instance showActionResp :: Show ActionResp where
  show = genericShow

instance shapeReadForeign :: JSON.ReadForeign Shape where
  readImpl = enumReadForeign

instance actionRespReadForeign :: JSON.ReadForeign ActionResp where
  readImpl = enumReadForeign

type Point =
  { x :: Number
  , y :: Number
  }

type Angle =
  { a :: Number
  }

type Object2 =
  { rotation :: Angle
  , center :: Point
  , velocity :: Angle
  , shape :: Shape
  }

type Object =
  { center :: Point
  , shape :: Shape
  , rotation :: Angle
  , velocity :: Point
  }

type Avatar =
  { object :: Object
  }

type Obstacle =
  { object :: Object
  }

newtype W a = W a

instance showW :: Show a => Show (W a) where
  show (W x) = show x

instance readMap :: JSON.ReadForeign a => JSON.ReadForeign (W (M.Map String a)) where
  readImpl = pure <<< W <<< objectToMap <=< JSON.read'
    where
      objectToMap :: forall a. FObject.Object a -> M.Map String a
      objectToMap x = M.fromFoldable ((FObject.toUnfoldable x) :: Array (Tuple String a))

type WMap a =  W (M.Map String a)

data ActionResp = GameWorldResp { avatars :: WMap Avatar, obstacles :: WMap Obstacle }

derive instance genericActionResp :: GR.Generic ActionResp _

readMessage :: String -> Either Foreign.MultipleErrors (Array ActionResp)
readMessage = JSON.readJSON
