{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module TREXIO.Internal.TH where

import Control.Monad
import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Bit hiding (Vector)
import Data.Bit qualified as BV
import Data.Char
import Data.Int
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Massiv.Array as Massiv hiding (Dim, forM, toList)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Foreign
import GHC.Generics (Generic)
import TREXIO.CooArray
import Text.Casing
import Text.Read (readMaybe)

-- import TREXIO.Internal.Bindings
import Foreign.C.Types
import Foreign.Ptr
import Language.Haskell.TH
import Language.Haskell.TH (DerivClause)
import Debug.Trace (traceM)

tshow :: (Show a) => a -> Text
tshow = T.pack . show

--------------------------------------------------------------------------------

newtype TrexioScheme = TrexioScheme (Map GroupName Group)
  deriving (Generic, Show, Eq, Ord)
  deriving (ToJSON, FromJSON) via Map GroupName Group

newtype GroupName = GroupName Text
  deriving (Generic, Show, Eq, Ord)
  deriving (ToJSONKey, FromJSONKey) via Text

newtype Group = Group (Map DataName Typ)
  deriving (Generic, Show, Eq, Ord)
  deriving (ToJSON, FromJSON) via Map DataName Typ

newtype DataName = DataName Text
  deriving (Generic, Show, Eq, Ord)
  deriving (ToJSONKey, FromJSONKey) via Text

instance ToJSON DataName where
  toJSON (DataName name) = String name

instance FromJSON DataName where
  parseJSON (String name) = return . DataName $ name
  parseJSON _ = fail "parseJSON(DataName): could not parse"

data Typ
  = -- | A 32 integer but meant to represent the size in a given dimension. The
    -- Bool indicates if field can also be written
    Dim Bool Length
  | -- | A 32 bit integer
    Int Length
  | -- | A double precision float. The Bool indicates whether this field is
    -- buffered
    Float Bool Length
  | -- | A string with a given length
    Str Length
  | -- | An index type
    Idx Length
  | -- | Sparse array of floats
    SparseFloat Length
  | -- | A bit field
    BitField Length
  deriving (Generic, Show, Eq, Ord)

instance ToJSON Typ where
  toJSON (Dim False len) = Array ["dim", toJSON len]
  toJSON (Dim True len) = Array ["dim readonly", toJSON len]
  toJSON (Int len) = Array ["int", toJSON len]
  toJSON (Float False len) = Array ["float", toJSON len]
  toJSON (Float True len) = Array ["float buffered", toJSON len]
  toJSON (Str len) = Array ["str", toJSON len]
  toJSON (Idx len) = Array ["index", toJSON len]
  toJSON (SparseFloat len) = Array ["float sparse", toJSON len]
  toJSON (BitField len) = Array ["int special", toJSON len]

instance FromJSON Typ where
  parseJSON (Array ["dim", len]) = Dim True <$> parseJSON len
  parseJSON (Array ["dim readonly", len]) = Dim False <$> parseJSON len
  parseJSON (Array ["int", len]) = Int <$> parseJSON len
  parseJSON (Array ["float", len]) = Float False <$> parseJSON len
  parseJSON (Array ["float buffered", len]) = Float True <$> parseJSON len
  parseJSON (Array ["str", len]) = Str <$> parseJSON len
  parseJSON (Array ["index", len]) = Idx <$> parseJSON len
  parseJSON (Array ["float sparse", len]) = SparseFloat <$> parseJSON len
  parseJSON (Array ["int special", len]) = BitField <$> parseJSON len
  parseJSON _ = fail "parseJSON(Typ): could not parse"

newtype Length = Length [DimLength] deriving (Generic, Show, Eq, Ord)

instance ToJSON Length where
  toJSON (Length dim) = Array . V.fromList . fmap toJSON $ dim

instance FromJSON Length where
  parseJSON (Array arr) =
    Length . V.toList
      <$> traverse (parseJSON @DimLength) arr
  parseJSON _ = fail "parseJSON(Length): could not parse"

data DimLength
  = Const Int
  | Field GroupName DataName
  deriving (Generic, Show, Eq, Ord)

instance ToJSON DimLength where
  toJSON (Const int) = String . tshow $ int
  toJSON (Field (GroupName groupName) (DataName dataName)) = String $ groupName <> "." <> dataName

instance FromJSON DimLength where
  parseJSON (String s) = case readMaybe . T.unpack $ s of
    Just i -> return . Const $ i
    Nothing -> case T.splitOn "." s of
      [groupName, dataName] -> return $ Field (GroupName groupName) (DataName dataName)
  parseJSON _ = fail "parseJSON(DimLength): could not parse"

--------------------------------------------------------------------------------
-- Helper functions

-- | Associate a 'Typ' with a 'Type'.
typToType :: (Quote m) => Typ -> m Type
typToType (Dim _ (Length [])) = [t|Int|]
typToType (Dim _ (Length [_])) = [t|Vector S Int|]
typToType (Int (Length [])) = [t|Int|]
typToType (Int (Length [_])) = [t|Vector S Int|]
typToType (Float _ (Length [])) = [t|Double|]
typToType (Float _ (Length [_])) = [t|Vector S Double|]
typToType (Float _ (Length [_, _])) = [t|Matrix S Double|]
typToType (Float _ (Length [_, _, _])) = [t|Massiv.Array S Ix3 Double|]
typToType (Float _ (Length [_, _, _, _])) = [t|Massiv.Array S Ix4 Double|]
typToType (Str (Length [])) = [t|Text|]
typToType (Str (Length [_])) = [t|Vector B Text|]
typToType (Idx (Length [])) = [t|Int|]
typToType (Idx (Length [_])) = [t|Vector S Int|]
typToType (SparseFloat (Length [_, _])) = [t|CooArray S Ix2 Double|]
typToType (SparseFloat (Length [_, _, _])) = [t|CooArray S Ix3 Double|]
typToType (SparseFloat (Length [_, _, _, _])) = [t|CooArray S Ix4 Double|]
typToType (SparseFloat (Length [_, _, _, _, _, _])) = [t|CooArray S (IxN 6) Double|]
typToType (SparseFloat (Length [_, _, _, _, _, _, _, _])) = [t|CooArray S (IxN 8) Double|]
typToType (BitField (Length [_])) = [t|BV.Vector Word8|]
typToType t = error $ "Can not associate " <> show t <> " with a Type"

{- | Sanitise an identifier, e.g. a field name or function name. I.e. we ensure
it starts with a valid lower case letter or symbol.
-}
sanId :: String -> String
sanId ind@(c : cs)
  | isUpperCase c = sanId $ toLower c : cs
  | not $ isAlpha c =  sanId $ '_' : c : cs
  | ind == "type" = "type'"
  | ind == "class" = "class'"
  | otherwise = c : cs
 
-- | Convert a field to a type
fieldToType :: (Quote m) => DataName -> Typ -> m VarBangType
fieldToType (DataName dataName) typ = do
  let fieldName = mkName . camel . sanId . T.unpack $ dataName
  fieldType <- typToType typ
  return (fieldName, Bang NoSourceUnpackedness SourceStrict, fieldType)

--------------------------------------------------------------------------------
-- Template Haskell binding generator

-- | The standard operations on data fields.
data FieldOps
  = Has
  | Read
  | Write
  deriving (Generic, Eq, Show, Ord)

stdDerivs :: [DerivClause]
stdDerivs = [DerivClause Nothing [ConT ''Generic, ConT ''Show, ConT ''Ord, ConT ''Eq]]

-- | Create a record from a given group
mkRecord :: GroupName -> Group -> Q Dec
mkRecord (GroupName groupName) (Group fields) = do
  reportWarning "Generating"
  groupNameTD <- newName . pascal . T.unpack $ groupName
  groupNameTC <- newName . pascal . T.unpack $ groupName
  fieldsT <- traverse (uncurry fieldToType) . Map.toList $ fields
  return $ DataD [] groupNameTD [] Nothing [RecC groupNameTC fieldsT] stdDerivs

{-
testName :: TrexioT -> Text
testName (TrexioT _) = "test"

-- | Make a reader function for scalars
mkReadInt :: Text -> Text -> Q [Dec]
mkReadInt groupName dataName = do
  -- Import the C function raw
  cFnNameT <- newName . T.unpack $ cFnName
  cSig <- [t|TrexioT -> Ptr Int32 -> IO Int32|]
  let cFn = ForeignD $ ImportF CApi Safe capiStr cFnNameT cSig

  reportWarning "@@@@@@@@@@@@@@@@@ Created C function"

  -- Make a Haskell wrapper around the C function
  hsSig <- [t|TrexioT -> IO Int32|]
  hsFnNameT <- newName . T.unpack $ hsFnName
  hsFnBody <-
    [e|
      \trexio -> alloca $ \resPtr -> do
        $(varE cFnNameT) trexio resPtr
      |]

  let hsFn = FunD hsFnNameT [Clause [] (NormalB hsFnBody) []]

  return [cFn, hsFn]
 where
  cFnName = "trexio_read_" <> groupName <> "_" <> dataName
  hsFnName = "read" <> T.toTitle groupName <> T.toTitle dataName
  capiStr = "trexio.h trexio_read_" <> T.unpack groupName <> "_" <> T.unpack dataName
-}