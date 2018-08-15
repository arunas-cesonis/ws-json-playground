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

data Shape =
    Square Number
  | Point
  | Circle Number
  | Rectangle Number Number
  | NamedCircle String Number
  | NamedRectangle String Number Number

derive instance genericShape :: GR.Generic Shape _

instance shapeReadForeign :: JSON.ReadForeign Shape where
  readImpl = enumReadForeign

enumReadForeign :: forall a rep
  . GR.Generic a rep
  => EnumReadForeign rep
  => Foreign
  -> Foreign.F a
enumReadForeign f =
  GR.to <$> enumReadForeignImpl f

class EnumReadForeign rep where
  enumReadForeignImpl :: Foreign -> Foreign.F rep

class EnumReadForeignProduct rep where
  enumReadForeignProductImpl :: Int -> Foreign -> Foreign.F rep

contents :: String
contents = "contents"

tag :: String
tag = "tag"

instance sumEnumReadForeign ::
  ( EnumReadForeign a
  , EnumReadForeign b
  ) => EnumReadForeign (GR.Sum a b) where
  enumReadForeignImpl f
    = GR.Inl <$> enumReadForeignImpl f
    <|> GR.Inr <$> enumReadForeignImpl f

instance argumentEnumReadForeign ::
  ( JSON.ReadForeign a
  ) => EnumReadForeign (GR.Argument a) where
  enumReadForeignImpl f = GR.Argument <$> (JSON.readImpl =<< readProp contents f)

instance noArgumentsEnumReadForeign :: EnumReadForeign GR.NoArguments where
  enumReadForeignImpl _ = pure $ GR.NoArguments

instance productEnumReadGenericProduct ::
  ( EnumReadForeignProduct a
  , EnumReadForeignProduct b
  ) => EnumReadForeignProduct (GR.Product a b) where
  enumReadForeignProductImpl i f = do
      GR.Product
      <$> enumReadForeignProductImpl i f
      <*> enumReadForeignProductImpl (i + 1) f

instance productEnumReadGenericArgument ::
  ( JSON.ReadForeign a
  ) => EnumReadForeignProduct (GR.Argument a) where
  enumReadForeignProductImpl i f = do
      GR.Argument <$> (JSON.readImpl =<< readIndex i f)

instance productEnumReadForeign ::
  ( EnumReadForeignProduct (GR.Product a b)
  ) => EnumReadForeign (GR.Product a b) where
  enumReadForeignImpl f = enumReadForeignProductImpl 0 =<< readProp contents f

instance constructorEnumReadForeignPN ::
  ( IsSymbol name
  , EnumReadForeign a
  ) => EnumReadForeign (GR.Constructor name a) where
  enumReadForeignImpl f = do
    tag <- readString =<< readProp tag f
    if tag == name
      then GR.Constructor <$> enumReadForeignImpl f
      else throwError <<< pure <<< Foreign.ForeignError $ "BAM 3"
    where
      name = reflectSymbol (SProxy :: SProxy name)

readShape :: String -> Either Foreign.MultipleErrors Shape
readShape = JSON.readJSON

instance showShape :: Show Shape where
  show = genericShow
