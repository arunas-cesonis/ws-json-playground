module Message
  ( readMessage
  , ActionResp(..)
  , emptyGameWorld
  , GameWorld
  , TransportId
  , W(..)
  , WMap
  , Avatar
  , Obstacle
  , Object
  , Point
  , Shape(..)
  , Angle
  )
  where

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

data TransportId =
    TAvatarId Int
  | TObstacleId Int

derive instance genericShape :: GR.Generic Shape _
derive instance genericTransportId :: GR.Generic TransportId _

instance showShape :: Show Shape where
  show = genericShow

instance showActionResp :: Show ActionResp where
  show = genericShow

instance showTransportId :: Show TransportId where
  show = genericShow

instance shapeReadForeign :: JSON.ReadForeign Shape where
  readImpl = enumReadForeign

instance transportIdReadForeign :: JSON.ReadForeign TransportId where
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
type GameWorld = { avatars :: WMap Avatar, obstacles :: WMap Obstacle }

data ActionResp = GameWorldResp GameWorld
                | MoveResp TransportId Point
                | RotateResp TransportId Angle

emptyGameWorld :: GameWorld
emptyGameWorld =
  { avatars : W M.empty
  , obstacles : W M.empty
  }

derive instance genericActionResp :: GR.Generic ActionResp _

readMessage :: String -> Either Foreign.MultipleErrors (Array ActionResp)
readMessage = JSON.readJSON
