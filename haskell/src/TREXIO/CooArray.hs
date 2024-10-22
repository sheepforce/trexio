module TREXIO.CooArray (
  CooArray,
  values,
  coords,
  mkCooArrayF,
  mkCooArray,
)
where

import Data.Foldable
import Data.Massiv.Array as Massiv hiding (toList)

-- | A coordinate list array representation.
data CooArray r ix a = CooArray
  { values :: Vector r a
  , coords :: Vector r ix
  }

-- | Make a 'CooArray' from a list of coordinate-value pairs.
mkCooArrayF ::
  (Foldable f, Index ix, Manifest r a, Manifest r ix) =>
  f (ix, a) ->
  CooArray r ix a
mkCooArrayF coo = CooArray{..}
 where
  arr = Massiv.fromList @B Par . toList $ coo
  values = Massiv.compute . Massiv.map snd $ arr
  coords = Massiv.compute . Massiv.map fst $ arr

-- | Make a 'CooArray' from a indices and vectors.
mkCooArray ::
  (MonadThrow m, Index ix, Size r) =>
  Vector r ix ->
  Vector r a ->
  m (CooArray r ix a)
mkCooArray coords values
  | Massiv.size coords == Massiv.size values = return CooArray{..}
  | otherwise = throwM $ SizeMismatchException (Massiv.size coords) (Massiv.size values)