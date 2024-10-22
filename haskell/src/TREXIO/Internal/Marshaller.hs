module TREXIO.Internal.Marshaller where

import Data.Massiv.Array as Massiv hiding (withMArray)
import Data.Massiv.Array.Unsafe
import Foreign
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import GHC.Weak (finalize)

{- | Passes a mutable 'MArray' to a C function. The array may be deallocated
on C side or modified in place. This funciton is super dangerous. You array may
suddenly be gone!
-}
withMArray :: (Index ix) => MArray s S ix a -> (Ptr b -> IO c) -> IO c
withMArray v f = do
  let (arrFPtr, _arrL) = unsafeMArrayToForeignPtr v
  withForeignPtr arrFPtr $ \arrPtr -> f (castPtr arrPtr)

{- | Pass a 'Massiv.Array' to a C function. This function is safe, but the array
is copied to a new memory location.
-}
withArray :: (Storable a, Index ix) => Array S ix a -> (Ptr b -> IO c) -> IO c
withArray v f = do
  mArr <- thaw v
  withMArray mArr f

-- | Get an 'MArray' from C memory. Haskell and C side use the same memory reference.
peekMArray :: (Index ix, Storable a) => Sz ix -> Ptr a -> IO (MArray s S ix a)
peekMArray sz ptr = do
  fPtr <- newForeignPtr finalizerFree ptr
  let mArr = unsafeMArrayFromForeignPtr0 fPtr (toLinearSz sz)
  resizeMArrayM sz mArr

-- | Get an 'Array' from C memory. The underlying C array is copied and the
-- result is safe to use.
peekArray :: (Index ix, Storable a) => Sz ix -> Ptr a -> IO (Array S ix a)
peekArray sz ptr = do
  mArr <- peekMArray sz ptr
  freeze Par mArr