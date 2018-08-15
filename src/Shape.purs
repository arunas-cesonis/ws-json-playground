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
    Square Number
  | Point
  | Circle Number
  | Rectangle Number Number
  | NamedCircle String Number

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
  , JSON.ReadForeign a
  ) => EnumReadForeign (GR.Constructor name (GR.Argument a)) where
  enumReadForeignImpl f = do
    s :: { tag :: String, contents :: a } <- JSON.readImpl f
    if s.tag == name
      then pure $ GR.Constructor (GR.Argument s.contents)
      else throwError <<< pure <<< Foreign.ForeignError $ "BAM 1"
    where
      name = reflectSymbol (SProxy :: SProxy name)

instance argumentEnumReadForeign ::
  ( JSON.ReadForeign a
  ) => EnumReadForeign (GR.Argument a) where
  enumReadForeignImpl f = GR.Argument <$> JSON.readImpl f

instance constructorEnumReadForeignPN ::
  ( IsSymbol name
  , JSON.ReadForeign a
  , JSON.ReadForeign b
  ) => EnumReadForeign (GR.Constructor name (GR.Product (GR.Argument a) (GR.Argument b))) where
  enumReadForeignImpl f = do
    s :: { tag :: String, contents :: Array Foreign } <- JSON.readImpl f
    if s.tag == name
      then ok s.contents
      else throwError <<< pure <<< Foreign.ForeignError $ "BAM 3"
    where
      name = reflectSymbol (SProxy :: SProxy name)
      ok [w, h] = do
        ww <- enumReadForeignImpl w
        hh <- enumReadForeignImpl h
        pure $ GR.Constructor (GR.Product ww hh)
      ok _ = throwError <<< pure <<< Foreign.ForeignError $ "BAM 4"

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
