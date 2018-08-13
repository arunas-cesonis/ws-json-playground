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

derive instance genericShape :: GR.Generic Shape _

class UntaggedSumRep rep where
  untaggedSumRep :: Foreign -> Foreign.F rep

instance untaggedSumRepSum :: (UntaggedSumRep a, UntaggedSumRep b) => UntaggedSumRep (GR.Sum a b) where
  untaggedSumRep f = GR.Inl <$> untaggedSumRep f 
                 <|> GR.Inr <$> untaggedSumRep f

instance untaggedSumRepConstructor ::
  ( UntaggedSumRep a
  ) => UntaggedSumRep (GR.Constructor name a) where
  untaggedSumRep f = GR.Constructor <$> untaggedSumRep f

instance untaggedSumRepArgument ::
  ( JSON.ReadForeign a
  ) => UntaggedSumRep (GR.Argument a) where
  untaggedSumRep f = GR.Argument <$> JSON.readImpl f

instance shapeReadForeign :: JSON.ReadForeign Shape where
  readImpl = enumReadForeign

enumReadForeign :: forall a rep
  . GR.Generic a rep
  => EnumReadForeign rep
  => Foreign
  -> Foreign.F a
enumReadForeign f = GR.to <$> enumReadForeignImpl f

class EnumReadForeign rep where
  enumReadForeignImpl :: Foreign -> Foreign.F rep

instance sumEnumReadForeign :: (EnumReadForeign a, EnumReadForeign b) => EnumReadForeign (GR.Sum a b) where
  enumReadForeignImpl f = GR.Inl <$> enumReadForeignImpl f <|> GR.Inr <$> enumReadForeignImpl f

instance showShape :: Show Shape where
  show = genericShow
