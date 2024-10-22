{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module TREXIO.Internal.TH where

import Control.Monad
import Data.Int
import Data.Text (Text)
import Data.Text qualified as T
import Foreign

-- import TREXIO.Internal.Bindings
import Foreign.C.Types
import Foreign.Ptr
import Language.Haskell.TH

-- | The TREXIO data type
newtype TrexioT = TrexioT (Ptr TrexioT)

--------------------------------------------------------------------------------
-- Template Haskell binding generator

-- | The standard operations on data groups.
ops :: [String]
ops = ["read", "write", "has"]

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

--------------------------------------------------------------------------------

nucleusQ :: Q [Dec]
nucleusQ = do
  fnT <- [t|CInt|]
  mkReadInt "nucleus" "num"
 where
  groupName = "nucleus"
  dataNames = []