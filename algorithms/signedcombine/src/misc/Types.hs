{-# LANGUAGE GeneralisedNewtypeDeriving, DerivingStrategies, ConstraintKinds, FlexibleContexts, TypeFamilies #-}

module Types where

import Control.Exception.Base (Exception(..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Random (evalRandT, RandT, MonadRandom)
import Data.Array (Ix)
import Data.Hashable (Hashable)
import Data.Bits (Bits)
import Numeric.Natural (Natural)
import System.Random (Random, StdGen, getStdGen)
import Text.Printf (printf, PrintfArg)

-- Monad stack (pretty simple only IO and Random)
newtype RandIO a = RandIO {unRandIO :: RandT StdGen IO a} deriving (Functor, Applicative, Monad, MonadRandom, MonadIO)
type Mon = RandIO

runRandIO :: RandIO a -> IO a
runRandIO = (getStdGen >>=) . evalRandT . unRandIO
runMon :: Mon a -> IO a
runMon = runRandIO

data CustomException = IndexError Idx String | DuplicationPresentError String | ReplicationPresentError String | GenomesWithSign String | InvalidOperation String String | TooFillMaps String | InvalidArgument String String | Uninitialized String | NotImplemented String | OtherError String String deriving (Show)
instance Exception CustomException where
  displayException (IndexError i str) = "Invalid Index " ++ show i ++ " on " ++ str ++ "."
  displayException (DuplicationPresentError str) = "Genome without duplicates" ++ " on " ++ str ++ "."
  displayException (ReplicationPresentError str) = "Genome with only duplicates are necessary" ++ " on " ++ str ++ "."
  displayException (GenomesWithSign str) = "Genomes must be unsigned" ++ " on " ++ str ++ "."
  displayException (InvalidOperation op str) = op ++ " not allowed in the Rearrangement Model" ++ " on " ++ str ++ "."
  displayException (TooFillMaps str) = "Too fill maps in the end of " ++ str ++ "."
  displayException (InvalidArgument arg str) = "Invalid argument " ++ arg ++ " on " ++ str ++ "."
  displayException (Uninitialized str) = "Uninitialized value" ++ " on " ++ str ++ "."
  displayException (NotImplemented str) = "Code not implemented" ++ " on " ++ str ++ "."
  displayException (OtherError err str) = err ++ " on " ++ str ++ "."

patternError :: String
patternError = "ERROR: Pattern shouldn't be possible."

type PrefixSize = Int
type SuffixSize = Int

-- wrappers for Int
newtype Dist = Dist Int deriving newtype (Num,Ord,Eq,Real,Enum,Integral,Show,Read,Bounded)
newtype Size = Size Int deriving newtype (Num,Ord,Eq,Real,Enum,Integral,Show,Read)
newtype Idx = Idx Int deriving newtype (Num,Ord,Eq,Real,Enum,Integral,Show,Read,Ix)
newtype Dup = Dup Int deriving newtype (Num,Ord,Eq,Real,Enum,Integral,Show,Read)
newtype Rep = Rep Int deriving newtype (Num,Ord,Eq,Real,Enum,Integral,Show,Read)
newtype Occ = Occ Int deriving newtype (Num,Ord,Eq,Real,Enum,Integral,Show,Read)

data Ori = LR | RL deriving (Eq,Show)
data Aprox = Ap3 | Ap4 deriving (Eq, Read, Show)
data RearrangeModel = Rev | Trans | TransRev deriving (Read, Eq, Show, Enum)
data IsSign = Signed | Unsigned deriving (Eq)

class Orientable o where
    getOri :: o -> Ori -- get orientation, if only one return LR
    invOri :: o -> o -- if has orientation invert it, otherwise do nothing

canonicOri :: (Orientable o) => o -> o
-- Transform to canonical orientation (LR).
canonicOri o = if getOri o == LR then o else invOri o

instance Orientable Int where
    getOri a = if a > 0 then LR else RL 
    invOri = negate

instance Orientable Natural where
    getOri _ = LR
    invOri = id

newtype BitMask = BM Integer deriving (Eq, Ord, Bits, Random, PrintfArg, Hashable)
instance Show BitMask where
  show = printf "%llb"

orient :: (Orientable o) => Ori -> o -> o
-- Try to transform o to have given orientation.
orient ori o = if ori == getOri o then o else invOri o

combineOrientation :: (Orientable o) => Ori -> o -> o
-- + + = +
-- + - = -
-- - + = -
-- - - = +
combineOrientation ori o =
    case ori of
      LR -> o
      RL -> invOri o

-- captuer the wrapping of some date in a new type
class Wrapped w where
    type Unwrapped w
    wrap :: Unwrapped w -> w
    unwrap :: w -> Unwrapped w
