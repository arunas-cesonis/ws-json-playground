module Shape(Shape(..), readShape) where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Except (throwError)
import Data.Generic.Rep as GR
import Data.Either
import Data.Generic.Rep.Show (genericShow)
import Type.Prelude (class IsSymbol, SProxy(..), reflectSymbol)

import Simple.JSON as JSON

import Foreign as Foreign
import Foreign (Foreign)

data Shape =
    Square
  | Circle Number
  | Rectangle


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

instance sumEnumReadForeign ::
  ( EnumReadForeign a
  , EnumReadForeign b
  ) => EnumReadForeign (GR.Sum a b) where
  enumReadForeignImpl f
    = GR.Inl <$> enumReadForeignImpl f
    <|> GR.Inr <$> enumReadForeignImpl f

instance constructorEnumReadForeignN ::
  ( IsSymbol name
  ) => EnumReadForeign (GR.Constructor name (GR.Argument Number)) where
  enumReadForeignImpl f = do
    s :: { tag :: String, contents :: Number } <- JSON.readImpl f
    if s.tag == name
      then pure $ GR.Constructor (GR.Argument s.contents)
      else throwError <<< pure <<< Foreign.ForeignError $ "BAM 1"
    where
      name = reflectSymbol (SProxy :: SProxy name)

instance constructorEnumReadForeign ::
  ( IsSymbol name
  ) => EnumReadForeign (GR.Constructor name GR.NoArguments) where
  enumReadForeignImpl f = do
    s :: { tag :: String } <- JSON.readImpl f
    if s.tag == name
      then pure $ GR.Constructor GR.NoArguments
      else throwError <<< pure <<< Foreign.ForeignError $ "BAM 2"
    where
      name = reflectSymbol (SProxy :: SProxy name)

readShape :: String -> Either Foreign.MultipleErrors Shape
readShape = JSON.readJSON

instance showShape :: Show Shape where
  show = genericShow
