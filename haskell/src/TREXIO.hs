{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}


module TREXIO where

import Control.Exception
import Control.Monad
import Data.Int
import Data.Massiv.Array as Massiv hiding (forM)
import Data.Text (Text)
import Data.Text qualified as T
import Foreign
import Foreign.C.String
import Foreign.C.Types
import GHC.Generics (Generic)
import Language.Haskell.TH
import System.OsPath
import TREXIO.CooArray
import TREXIO.Internal.Marshaller
import TREXIO.Internal.Bindings

--nucleusQ

