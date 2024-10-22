{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

#include <trexio.h>
{# context prefix = "trexio" #}

module TREXIO.Internal.Bindings where

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

{# typedef int32_t Int32 #}
{# default in `Int32' [int32_t] fromIntegral #}
{# default out `Int32' [int32_t] fromIntegral #}

--------------------------------------------------------------------------------

-- | Library version (major, minor, patch)
version :: (Int, Int, Int)
version =
  ( {#const TREXIO_VERSION_MAJOR #}
  , {#const TREXIO_VERSION_MINOR #}
  , {#const TREXIO_VERSION_PATCH #}
  )

--------------------------------------------------------------------------------

{# typedef trexio_exit_code Int32 #}
{# default in `ExitCode' [trexio_exit_code] exitCodeInMarsh #}
{# default out `ExitCode' [trexio_exit_code] exitCodeSafeOut* #}

exitCodeInMarsh :: ExitCode -> Int32
exitCodeInMarsh c = fromIntegral . fromEnum $ c

exitCodeOutMarsh :: Int32 -> ExitCode
exitCodeOutMarsh i = toEnum . fromIntegral $ i

-- | An output marshaller, that throws and exception on any non-successful exit code
exitCodeSafeOut :: Int32 -> IO ExitCode
exitCodeSafeOut i = do
  let c = exitCodeOutMarsh i
  if c == Success
    then return c
    else throwIO c

peekExitCode :: Ptr Int32 -> IO ExitCode
peekExitCode = fmap exitCodeOutMarsh . peek

-- | Exit Cod
data ExitCode
  = Failure
  | Success
  | InvalidArg1
  | InvalidArg2
  | InvalidArg3
  | InvalidArg4
  | InvalidArg5
  | End
  | ReadOnly
  | Errno
  | InvalidID
  | AllocationFailed
  | HasNot
  | InvalidNum
  | AttrAlreadyExists
  | DSetAlreadyExists
  | OpenError
  | LockError
  | UnlockError
  | FileError
  | GroupReadError
  | GroupWriteError
  | ElemReadError
  | ElemWriteError
  | UnsafeArrayDim
  | AttrMissing
  | DSetMissing
  | BackEndMissing
  | InvalidArg6
  | InvalidArg7
  | InvalidArg8
  | InvalidStrLen
  | IntSizeOverflow
  | SafeMode
  | InvalidElectronNum
  | InvalidDeterminantNum
  | InvalidState
  | VersionParsingIssue
  | PhaseChange
  deriving (Show, Eq, Ord, Generic)

instance Enum ExitCode where
  fromEnum Failure = -1
  fromEnum Success = 0
  fromEnum InvalidArg1 = 1
  fromEnum InvalidArg2 = 2
  fromEnum InvalidArg3 = 3
  fromEnum InvalidArg4 = 4
  fromEnum InvalidArg5 = 5
  fromEnum End = 6
  fromEnum ReadOnly = 7
  fromEnum Errno = 8
  fromEnum InvalidID = 9
  fromEnum AllocationFailed = 10
  fromEnum HasNot = 11
  fromEnum InvalidNum = 12
  fromEnum AttrAlreadyExists = 13
  fromEnum DSetAlreadyExists = 14
  fromEnum OpenError = 15
  fromEnum LockError = 16
  fromEnum UnlockError = 17
  fromEnum FileError = 18
  fromEnum GroupReadError = 19
  fromEnum GroupWriteError = 20
  fromEnum ElemReadError = 21
  fromEnum ElemWriteError = 22
  fromEnum UnsafeArrayDim = 23
  fromEnum AttrMissing = 24
  fromEnum DSetMissing = 25
  fromEnum BackEndMissing = 26
  fromEnum InvalidArg6 = 27
  fromEnum InvalidArg7 = 28
  fromEnum InvalidArg8 = 29
  fromEnum InvalidStrLen = 30
  fromEnum IntSizeOverflow = 31
  fromEnum SafeMode = 32
  fromEnum InvalidElectronNum = 33
  fromEnum InvalidDeterminantNum = 34
  fromEnum InvalidState = 35
  fromEnum VersionParsingIssue = 36
  fromEnum PhaseChange = 37

  toEnum (-1) = Failure
  toEnum 0 = Success
  toEnum 1 = InvalidArg1
  toEnum 2 = InvalidArg2
  toEnum 3 = InvalidArg3
  toEnum 4 = InvalidArg4
  toEnum 5 = InvalidArg5
  toEnum 6 = End
  toEnum 7 = ReadOnly
  toEnum 8 = Errno
  toEnum 9 = InvalidID
  toEnum 10 = AllocationFailed
  toEnum 11 = HasNot
  toEnum 12 = InvalidNum
  toEnum 13 = AttrAlreadyExists
  toEnum 14 = DSetAlreadyExists
  toEnum 15 = OpenError
  toEnum 16 = LockError
  toEnum 17 = UnlockError
  toEnum 18 = FileError
  toEnum 19 = GroupReadError
  toEnum 20 = GroupWriteError
  toEnum 21 = ElemReadError
  toEnum 22 = ElemWriteError
  toEnum 23 = UnsafeArrayDim
  toEnum 24 = AttrMissing
  toEnum 25 = DSetMissing
  toEnum 26 = BackEndMissing
  toEnum 27 = InvalidArg6
  toEnum 28 = InvalidArg7
  toEnum 29 = InvalidArg8
  toEnum 30 = InvalidStrLen
  toEnum 31 = IntSizeOverflow
  toEnum 32 = SafeMode
  toEnum 33 = InvalidElectronNum
  toEnum 34 = InvalidDeterminantNum
  toEnum 35 = InvalidState
  toEnum 36 = VersionParsingIssue
  toEnum 37 = PhaseChange
  toEnum _ = error "toEnum(ExitCode): invalid argument"

instance Exception ExitCode where
  displayException = stringOfError 

-- | Convert an 'ExitCode' to a string
{# fun pure string_of_error as ^
{ `ExitCode'
} -> `String'
#}

--------------------------------------------------------------------------------

{# typedef back_end_t Int32 #}
{# default in `BackEnd' [back_end_t] backEndInMarsh #}
{# default out `BackEnd' [back_end_t] backEndOutMarsh #}

backEndInMarsh :: BackEnd -> Int32
backEndInMarsh c = fromIntegral . fromEnum $ c

backEndOutMarsh :: Int32 -> BackEnd
backEndOutMarsh i = toEnum . fromIntegral $ i

data BackEnd
  = Hdf5
  | Text
  | Invalid
  deriving (Eq, Show, Ord, Generic)

instance Enum BackEnd where
  fromEnum Hdf5 = 0
  fromEnum Text = 1
  fromEnum Invalid = 2

  toEnum 0 = Hdf5
  toEnum 1 = Text
  toEnum (2) = Invalid
  toEnum _ = error "toEnum(BackEnd): invalid argument"

{# fun has_backend as ^
{ backEndInMarsh `BackEnd'
} -> `Bool'
#}

--------------------------------------------------------------------------------

data Mode
  = Read
  | Write
  deriving (Eq, Show, Ord)

modeInMarsh :: Mode -> CChar
modeInMarsh Read = castCharToCChar 'r'
modeInMarsh Write = castCharToCChar 'w'

--------------------------------------------------------------------------------

-- | A TREXIO data type
{# pointer *trexio_t as TrexioT newtype #} deriving (Eq)

{# fun open as open_
{ `String' -- ^ file name
, modeInMarsh `Mode' -- ^ mode
, `BackEnd' -- ^ back end
, alloca- `ExitCode' peekExitCode*
} -> `TrexioT'
#}

open :: OsPath -> Mode -> BackEnd -> IO TrexioT
open fp mode be = do
  fp' <- decodeUtf fp
  (t, ec) <- open_ fp' mode be
  if ec == Success
    then return t
    else throwIO ec

{# fun set_one_based as ^
{ `TrexioT'
} -> `ExitCode'
#}

{# fun close as ^
{ `TrexioT'
} -> `ExitCode'
#}

{# fun flush as ^
{ `TrexioT'
} -> `ExitCode'
#}

{# fun cp as ^
{ `String' -- ^ source path
, `String' -- ^ destination path
} -> `ExitCode'
#}

{# fun set_state as ^
{ `TrexioT'
, `Int32' -- ^ electronic state
} -> `ExitCode'
#}

{# fun get_state as ^
{ `TrexioT'
, alloca- `Int32' peek* -- ^ electronic state
} -> `ExitCode'
#}

{# fun info as ^
{} -> `ExitCode'
#}

--------------------------------------------------------------------------------

{# typedef bitfield_t Int64 #}
{# default in `Int64' [bitfield_t] fromIntegral #}
{# default out `Int64' [bitfield_t] fromIntegral #}

--------------------------------------------------------------------------------

{# fun delete_metadata as ^
{ `TrexioT'
} -> `ExitCode'
#}

{# fun delete_nucleus as ^
{ `TrexioT'
} -> `ExitCode'
#}

{# fun delete_cell as ^
{ `TrexioT'
} -> `ExitCode'
#}

{# fun delete_pbc as ^
{ `TrexioT'
} -> `ExitCode'
#}

{# fun delete_electron as ^
{ `TrexioT'
} -> `ExitCode'
#}

{# fun delete_state as ^
{ `TrexioT'
} -> `ExitCode'
#}

{# fun delete_basis as ^
{ `TrexioT'
} -> `ExitCode'
#}

{# fun delete_ecp as ^
{ `TrexioT'
} -> `ExitCode'
#}

{# fun delete_grid as ^
{ `TrexioT'
} -> `ExitCode'
#}

{# fun delete_ao as ^
{ `TrexioT'
} -> `ExitCode'
#}

{# fun delete_ao_1e_int as ^
{ `TrexioT'
} -> `ExitCode'
#}

{# fun delete_ao_2e_int as ^
{ `TrexioT'
} -> `ExitCode'
#}

{# fun delete_mo as ^
{ `TrexioT'
} -> `ExitCode'
#}

{# fun delete_mo_1e_int as ^
{ `TrexioT'
} -> `ExitCode'
#}

{# fun delete_mo_2e_int as ^
{ `TrexioT'
} -> `ExitCode'
#}

{# fun delete_determinant as ^
{ `TrexioT'
} -> `ExitCode'
#}

{# fun delete_csf as ^
{ `TrexioT'
} -> `ExitCode'
#}

{# fun delete_amplitude as ^
{ `TrexioT'
} -> `ExitCode'
#}

{# fun delete_rdm as ^
{ `TrexioT'
} -> `ExitCode'
#}

{# fun delete_jastrow as ^
{ `TrexioT'
} -> `ExitCode'
#}

{# fun delete_qmc as ^
{ `TrexioT'
} -> `ExitCode'
#}

data Nucleus = Nucleus
  { num :: Sz1                -- ^ Number of nuclei
  , charge :: Vector S Double -- ^ Charge of nuclei
  , coord :: Matrix S Double  -- ^ Coordinates of nuclei as \( n \times 3 \) matrix
  , label :: Vector B Text    -- ^ Atom labels
  , pointGroup :: Text        -- ^ Point group of the molecule
  , repulsion :: Double       -- ^ Nuclear repulsion energy
  }
  deriving (Show, Generic)

--------------------------------------------------------------------------------

data Cell = Cell
  { a :: Vector S Double  -- ^ Real-space lattice cell vector \( \mathbf{a} \)
  , b :: Vector S Double  -- ^ Real-space lattice cell vector \( \mathbf{b} \)
  , c :: Vector S Double  -- ^ Real-space lattice cell vector \( \mathbf{c} \)
  , gA :: Vector S Double -- ^ Reciprocal-space lattice cell vector \( \mathbf{g}_a \)
  , gB :: Vector S Double -- ^ Reciprocal-space lattice cell vector \( \mathbf{g}_b \)
  , gC :: Vector S Double -- ^ Reciprocal-space lattice cell vector \( \mathbf{g}_c \)
  , twoPi :: Bool         -- ^ If 'True', \( 2 \pi \) is included in the cell vectors
  }
  deriving (Show, Generic)

--------------------------------------------------------------------------------

data Pbc = Pbc
  { periodic :: Bool                -- ^ If 'True', periodic boundary conditions are applied
  , kPointNum :: Sz1                -- ^ Number of k-points
  , kPoint :: Vector S Double       -- ^ k-point coordinates
  , kPointWeight :: Vector S Double -- ^ k-point weights
  }
  deriving (Show, Generic)

--------------------------------------------------------------------------------

data Electron = Electron
  { num :: Sz1      -- ^ Number of electrons
  , upNum :: Sz1    -- ^ Number of spin-up electrons
  , downNum :: Sz1  -- ^ Number of spin-down electrons
  }
  deriving (Show, Generic)

--------------------------------------------------------------------------------

data State = State
  { num :: Sz1                  -- ^ Number of electronic states
  , ind :: Ix1                  -- ^ Index of the electronic state, 0 is the ground state
  , energy :: Double            -- ^ Energy of the current electronic state
  , currentLabel :: Text        -- ^ Label of the current electronic state
  , label :: Vector B Text      -- ^ Label of all electronic states
  , fileName :: Vector B OsPath -- ^ Names of TREXIO files linked into the currnent one with data for other states
  }
  deriving (Show, Generic)

--------------------------------------------------------------------------------

data BasisType
  = Gaussian
  | Slater
  | Numerical
  | PW
  deriving (Show, Eq, Generic)

-- | The angular momentum of a shell
newtype AngMom = AngMom Word8
  deriving (Show, Eq, Ord, Generic, Storable)

data OscillationKind
  = Cos1
  | Cos2
  deriving (Show, Eq, Ord, Generic)

data Basis = Basis
  { basisType :: BasisType
  , primNum :: Sz1
  , shellNum :: Sz1
  , naoGridNum :: Sz1
  , interpCoeffCnt :: Sz1
  , nucleusIndex :: Vector S Ix1        -- ^ Mapping of shell number (index) to nucleus number
  , shellAngMom :: Vector S AngMom      -- ^ Angular momentum of each shell
  , shellFactor :: Vector S Double      -- ^ Normalisation factor for each shell
  , rPower :: Vector S Double           -- ^ Power to which \( r \) is raised \( n_s \)
  , naoGridStart :: Vector S Ix1        -- ^ Index of the first data point for a given numerical orbital
  , naoGridSize :: Vector B Sz1         -- ^ Number ofdata points per numerical orbital
  , shellIndex :: Vector S Ix1          -- ^ One-to-one correspondence between primitives and shell index
  , exponent :: Vector S Double         -- ^ Exponents of the primitives \( \gamma_{ks} \)
  , exponentIm :: Vector S Double       -- ^ Imaginary part of the exponents of the primitives \( \gamma_{ks} \)
  , coefficient :: Vector S Double      -- ^ Coefficients of the primitives \( a_{ks} \)
  , coefficientIm :: Vector S Double    -- ^ Imaginary part of the coefficients of the primitives \( a_{ks} \)
  , oscillationArg :: Vector S Double   -- ^ Additional argument to have oscillating orbitals \( \beta_{ks} \)
  , osciallationKind :: OscillationKind -- ^ Kind of Oscillating function
  , primtFactor :: Vector S Double      -- ^ Normalization coefficients for the primitives \( f_{ks} \)
  , eCut :: Double                      -- ^ Energy cutoff for plane-wave calculations
  , naoGridRadius :: Vector S Double    -- ^ Radii of grid points for numerical orbitals
  , naoGridPhi :: Vector S Double       -- ^ Wave function values for numerical orbitals
  , naoGridGrad :: Vector S Double      -- ^ Radial gradient of numerical orbitals
  , naoGridLap :: Vector S Double       -- ^ Laplacian of numerical orbitals
  , interpolatorKind :: Text            -- ^ Kind of spline, e.g. "Polynomial"
  , interpolatorPhi :: Matrix S Double  -- ^ Coefficients for numerical orbital interpolation function
  , interpolatorGrad :: Matrix S Double -- ^ Coefficients for numerical orbital gradient interpolation function
  , interpolatorLap :: Matrix S Double  -- ^ Coefficients for numerical orbital Laplacian interpolation function
  } deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------

data Ecp = Ecp
  { maxAngMomPlus1 :: Vector S AngMom -- ^ \( l_\text{max} + 1 \) one higher than the max angular momentum in the removed core orbitals
  , zCore :: Vector S Word8           -- ^ Number of core electrons to remove per atom
  , num :: Sz1                        -- ^ Total number of ECP functions for all atoms and all values of \( l \)
  , angMom :: Vector S AngMom         -- ^ One-to-one correspondence between ECP items and the angular momentum \( l \)
  , nucleusIndex :: Vector S Ix1      -- ^ One-to-one correspondence between ECP items and the atom index
  , exponent :: Vector S Double       -- ^ \( \alpha_{Aql} \) all ECP exponents
  , coefficient :: Vector S Double    -- ^ \( \beta_{Aql} \) all ECP coefficients
  , power :: Vector S Double          -- ^ \( n_{Aql} \) all ECP powers
  } deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------

data Grid = Grid
  { description :: Text          -- ^ Details about the used quadratures can go here
  , radPrecision :: Double       -- ^ Radial precision parameter (not used in some schemes like Krack-Köster)
  , num :: Sz1                   -- ^ Number of grid points
  , maxAngNum :: Sz1             -- ^ Maximum number of angular grid points (for pruning)
  , minAngNum :: Sz1             -- ^ Minimum number of angular grid points (for pruning)
  , coord :: Vector S Double     -- ^ Discretized coordinate space
  , weight :: Vector S Double    -- ^ Grid weights according to a given partitioning (e.g. Becke)
  , angNum :: Sz1                -- ^ Number of angular integration points (if used)
  , angCoord :: Vector S Double  -- ^  Discretized angular space (if used)
  , angWeight :: Vector S Double -- ^ Angular grid weights (if used)
  , radNum :: Sz1                -- ^ Number of radial integration points (if used)
  , radCoord :: Vector S Double  -- ^ Discretized radial space (if used)
  , radWeight :: Vector S Double -- ^ Radial grid weights (if used)
  } deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------

data AtomicOrbital = AtomicOrbital
  { cart :: Bool          -- ^ Cartesian or spherical harmonics
  , num :: Sz1            -- ^ Total number of atomic orbitals
  , shell :: Vector S Ix1 -- ^ Basis set shell for each AO
  , normalization :: Vector S Double -- ^ Normalization factor \( N_i \)
  } deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------

data Ao1eInt = Ao1eInt
  { overlap :: Matrix S Double           -- ^ \( \langle p | q \rangle \)
  , kinetic :: Matrix S Double           -- ^ \( \langle p | \hat{T}_e | q \rangle \)
  , potentialNE :: Matrix S Double       -- ^ \( \langle p | \hat{V}_{\text{ne}} | q \rangle \)
  , ecp :: Matrix S Double               -- ^ \( \langle p | \hat{V}_{\text{ECP}} | q \rangle \)
  , coreHamiltonian :: Matrix S Double   -- ^ \( \langle p | \hat{h} | q \rangle \)
  , overlapIm :: Matrix S Double         -- ^ \( \langle p | q \rangle \) (imaginary part)
  , kineticIm :: Matrix S Double         -- ^ \( \langle p | \hat{T}_e | q \rangle \) (imaginary part)
  , potentialNEIm :: Matrix S Double     -- ^ \( \langle p | \hat{V}_{\text{ne}} | q \rangle \) (imaginary part)
  , ecpIm :: Matrix S Double             -- ^ \( \langle p | \hat{V}_{\text{ECP}} | q \rangle \) (imaginary part)
  , coreHamiltonianIm :: Matrix S Double -- ^ \( \langle p | \hat{h} | q \rangle \) (imaginary part)
  } deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------

data Ao2eInt = Ao2eInt
  { eri :: CooArray S Ix4 Double           -- ^ Electron repulsion integrals, square N(AO)⁴
  , eriLR :: CooArray S Ix4 Double         -- ^ Long range ERIs
  , eriCholeskyNum :: Sz1                  -- ^ Number of Cholesky vectors
  , eriCholesky :: CooArray S Ix3 Double   -- ^ Cholesky decomposition of the ERIs, eriCholeskyNum x N(AO)²
  , eriLRCholeskyNum :: Sz1                -- ^ Number of Cholesky vectors for long range ERIs
  , eriLRCholesky :: CooArray S Ix3 Double -- ^ Cholesky decomposition of the long range ERIs, eriLRCholeskyNum x N(AO)²
  } deriving (Generic)

--------------------------------------------------------------------------------

data MoClass
  = Core
  | Inactive
  | Active
  | Virtual
  | Deleted
  deriving (Eq, Show, Generic)

data Spin
  = Alpha
  | Beta
  deriving (Eq, Show, Generic)

data MO = MO
  { moType :: Text                   -- ^ Type of MO, e.g. "HF", "Natural", "CASSCF", ...
  , num :: Sz1                       -- ^ Number of MOs
  , coefficient :: Matrix S Double   -- ^ MO coefficients, N(MO) x N(AO)
  , coefficientIm :: Matrix S Double -- ^ MO coefficients (imaginary part), N(MO) x N(AO)
  , moClass :: MoClass               -- ^ The type of MOs saved here
  , symmetry :: Text                 -- ^ Point group symmetry of the MOs
  , occupation :: Vector S Double    -- ^ Occupation numbers of the MOs
  , energy :: Vector S Double        -- ^ Energy of the MOs
  , spin :: Spin                     -- ^ Spin of the MOs
  , kPoint :: Vector S Double        -- ^ k-point coordinates
  }