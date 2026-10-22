{-# LANGUAGE ScopedTypeVariables, FlexibleContexts,
  TemplateHaskell, MultiParamTypeClasses, ConstraintKinds,
  DerivingStrategies, FlexibleInstances, GeneralizedNewtypeDeriving,
  TypeFamilies, ExistentialQuantification, UndecidableInstances, DataKinds,
  TypeApplications, AllowAmbiguousTypes #-}

module Instance
  ( RearrangeInstance(ModelOfInstance, cut, rev, trans)
  , ContractedGene
  , MRev
  , MTrans
  , MTransRev
  , AnyInstance(..)
  , ReaInstGenome
  , ReaInstGenMod
  , ReaInstAll
  , FullModel
  , FullGenome
  , RIStr
  , RIDup
  , RIPerm
  , OnlyDup
  , NoDup
  , ExtendedInstance
  , replaceGenomes
  , replaceGenomesSameType
  , buildInstance
  , getSource
  , getTarget
  , dup
  , rep
  , len
  , numOp
  , sig
  , unwrapInstance
  , unwrapInstanceAsLists
  , unwrapInstanceAsPerms
  , describe
  , getRepInfo
  , writeInstance
  , readInstance
  , readCorrectInstance
  , isPerm
  , onlyDup
  , RepStatus(..)
  , sameGenomes
  , empty
  , mirror
  , balanced
  , applyAssOnInstance
  , makeExtended
  , withoutExtencion
  , checkPerm
  , singleton
  , duplicated
  , replicated
  ) where

-----------------------------------------------------------------------
import Types
import Aux
-----------------------------------------------------------------------
import Genome (Gene(..), Genome, getOcc, getPerm, renumberDup)
import MappingRep (RepInfo(RepInfo))
import Perm (Perm)
-----------------------------------------------------------------------
import Control.Arrow (first)
import Control.Exception.Base (throw)
import Lens.Simple (Lens', (%~), (&), (+~), (-~), (.~), (^.), makeLenses, view)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Coerce (coerce)
import Data.IntMap (IntMap)
import Data.Maybe (fromMaybe)
-----------------------------------------------------------------------
import qualified Genome as G
import qualified Data.List as List
import qualified Data.IntMap as IntMap
-----------------------------------------------------------------------

---------- Rearrange Instance -----------------------

type ReaInstGenome inst g a = (RearrangeInstance inst, g ~ GenomeOfinst inst, a ~ GeneOfinst inst, Genome g a)
type ReaInstGenMod inst g a mod
     = ( RearrangeInstance inst
       , g ~ GenomeOfinst inst
       , a ~ GeneOfinst inst
       , Genome g a
       , mod ~ ModelOfInstance inst
       , Model mod)
type FullModel mod
    = ( Model mod
      , G.Gene (ContractedGene mod)
      , Orientable (Due mod G.SGene)
      , Orientable (Due mod G.UGene)
      , Orientable (Due mod (ContractedGene mod))
      , Orientable (Strip mod G.SGene)
      , Orientable (Strip mod G.UGene)
      , Orientable (Strip mod (ContractedGene mod)))
type FullGenome g a a'
     = ( G.Genome g a
       , G.Genome g G.UGene
       , G.Genome g G.SGene
       , G.Genome g a')
type ReaInstAll inst g a a' mod
     = ( ReaInstGenMod inst g a mod
       , a' ~ ContractedGene mod
       , FullModel mod
       , FullGenome g a a'
       , Orientable (Due mod a)
       , Orientable (Strip mod a))
type RIPerm mod g a = InstanceData mod IsPerm NotExt g a
type RIDup mod g a = InstanceData mod IsDup NotExt g a
type RIStr mod g a = InstanceData mod IsStr NotExt g a

class (Eq inst, Show inst) => RearrangeInstance inst where
  type GenomeOfinst inst :: * -> *
  type GeneOfinst inst :: *
  type ModelOfInstance inst :: *
  source :: Lens' inst (GenomeOfinst inst (GeneOfinst inst))
  target :: Lens' inst (GenomeOfinst inst (GeneOfinst inst))
  lengthOfInstance :: Lens' inst Size
  numOpOfInstance :: Lens' inst Dist
  numDupOfInstance :: Lens' inst Dup
  numRepOfInstance :: Lens' inst Rep
  cut :: PrefixSize -> SuffixSize -> inst -> inst
  rev :: RevOp -> inst -> inst
  trans :: TransOp -> inst -> inst

class OnlyDup instga

class NoDup instga

class ExtendedInstance instga

getSource :: (ReaInstGenome inst g a) => inst -> g a
getSource = view source

getTarget :: (ReaInstGenome inst g a) => inst -> g a
getTarget = view target

dup :: (RearrangeInstance inst) => inst -> Dup
dup = view numDupOfInstance

rep :: (RearrangeInstance inst) => inst -> Rep
rep = view numRepOfInstance

len :: (RearrangeInstance inst) => inst -> Size
len = view lengthOfInstance

numOp :: (RearrangeInstance inst) => inst -> Dist
numOp = view numOpOfInstance

unwrapInstance :: (ReaInstGenome inst g a) => inst -> (g a, g a, Size, Dup, Rep)
unwrapInstance inst = (inst ^. source, inst ^. target, inst ^. lengthOfInstance, inst ^. numDupOfInstance, inst ^. numRepOfInstance)

unwrapInstanceAsLists :: (ReaInstGenome inst g a) => inst -> ([a], [a], Size, Dup, Rep)
unwrapInstanceAsLists inst = (l1, l2, n, d, r)
  where
    (g1, g2, n, d, r) = unwrapInstance inst
    l1 = G.toListOfGenes g1
    l2 = G.toListOfGenes g2

unwrapInstanceAsPerms :: (ReaInstGenome inst g a) => inst -> (Perm, Perm, Size)
unwrapInstanceAsPerms inst = (getPerm g1, getPerm g2, n)
  where
    (g1,g2,n,_,_) = unwrapInstance inst

describe :: (ReaInstGenome inst g a) => inst -> (Size, Int, Dup, Rep, Occ)
describe inst = (len inst, G.sin . getSource $ inst, dup inst, rep inst, G.occGrt . getSource $ inst)

updateDup :: (ReaInstGenome inst g a) => inst -> inst
updateDup inst = inst & numDupOfInstance .~ G.dup (inst ^. source)

updateRep :: (ReaInstGenome inst g a) => inst -> inst
updateRep inst = inst & numRepOfInstance .~ G.rep (inst ^. source)

isPerm :: (ReaInstGenome inst g a) => inst -> Bool
isPerm = (== 0) . view numRepOfInstance

sig :: (ReaInstGenome inst g a) => inst -> Bool
sig = G.sig . view source

onlyDup :: (ReaInstGenome inst g a) => inst -> Bool
onlyDup inst = fromIntegral (inst ^. numDupOfInstance) == inst ^. numRepOfInstance

empty :: (ReaInstGenome inst g a) => inst -> Bool
empty inst = inst ^. lengthOfInstance == 0

mirror :: (ReaInstGenome inst g a) => inst -> inst
mirror inst = inst & source %~ G.mirror
                   & target %~ G.mirror

balanced :: (ReaInstGenome inst g a) => inst -> Bool
balanced inst =
  (G.alf g1 == G.alf g2) && (G.occA g1 == G.occA g2)
  where
    g1 = inst ^. source
    g2 = inst ^. target

sameGenomes :: (ReaInstGenome inst g a) => inst -> Bool
sameGenomes inst = view source inst == view target inst

applyAssOnInstance :: (ReaInstGenome inst g a) => (Assigments a, Assigments a) -> inst -> inst
applyAssOnInstance (ass1,ass2) inst =
  updateDup . updateRep $
    inst & source %~ fmap fromIntegral . applyAss ass1
         & target %~ fmap fromIntegral . applyAss ass2

getRepInfo :: (ReaInstGenome inst g a) => inst -> RepInfo
getRepInfo inst = RepInfo (len inst) (rep inst) (G.getOccList . G.occA $ g)
  where g = view source inst

----------------- Instances for RearrangeInstance ----------------------

data RepStatus = IsPerm | IsDup | IsStr deriving (Eq)
data Ext = IsExt | NotExt

data InstanceData m (r :: RepStatus) (e :: Ext) g a = InstanceData {
  _numOpOfInstanceData :: Dist,
  _sourceInstanceData :: g a,
  _targetInstanceData :: g a,
  _lengthOfInstanceData :: Size,
  _numDupOfInstanceData :: Dup,
  _numRepOfInstanceData :: Rep,
  _occMap :: IntMap Occ}
makeLenses ''InstanceData

data AnyInstance =
    forall m r g a a'. ( Genome g a
                       , a' ~ ContractedGene m
                       , Model m
                       , G.Genome g G.UGene
                       , G.Genome g G.SGene
                       , a' ~ ContractedGene m
                       , FullModel m
                       , Orientable (Due m a)
                       , Orientable (Strip m a)
                       , G.Genome g a'
                       ) =>
                       AnyInstance (InstanceData m r NotExt g a)

instance Show AnyInstance where
  show (AnyInstance inst) = show inst

instance Eq AnyInstance where
    (AnyInstance inst1) == (AnyInstance inst2) =
        (map fromIntegral . G.toListOfGenes $ view sourceInstanceData inst1) == (G.toListOfGenes $ view sourceInstanceData inst2) &&
        (map fromIntegral . G.toListOfGenes $ view targetInstanceData inst1) == (G.toListOfGenes $ view targetInstanceData inst2)

instance (Show (g a), Model m) => Show (InstanceData m r e g a) where
  show = showInstData
    where showInstData (InstanceData nop g1 g2 n d r _) =
            show (model @m) ++ " " ++ show g1 ++ " | " ++ show g2 ++ " | n:" ++
            show n ++ ",d:" ++ show d ++ ",r:" ++ show r ++ ",nop:" ++ show nop

instance (Genome g a) => Eq (InstanceData m r e g a) where
  inst1 == inst2 =
    view sourceInstanceData inst1 == view sourceInstanceData inst2 &&
    view targetInstanceData inst1 == view targetInstanceData inst2

instance (Show (g a), Genome g a, Model m) => RearrangeInstance (InstanceData m r e g a) where
  type GenomeOfinst (InstanceData m r e g a) = g
  type GeneOfinst (InstanceData m r e g a) = a
  type ModelOfInstance (InstanceData m r e g a) = m
  numOpOfInstance = numOpOfInstanceData
  source = sourceInstanceData
  target = targetInstanceData
  lengthOfInstance = lengthOfInstanceData
  numDupOfInstance = numDupOfInstanceData
  numRepOfInstance = numRepOfInstanceData
  cut prefix suffix inst =
    inst & source .~ g1'
         & target .~ g2'
         & lengthOfInstance .~ max 0 (n - fromIntegral (prefix + suffix))
         & numDupOfInstance +~ incDupl
         & numRepOfInstance +~ incRepl
         & occMap .~ om'
    where
      om_ = view occMap inst
      om = if IntMap.null om_ then om_init else om_
      (incDupl,incRepl,om') = List.foldl' testAndUpdateOne (0,0,om) possibleRepl
      (g1 ,g2 , n, _, r) = unwrapInstance inst

      (g1', removed) = G.cut prefix suffix g1
      (g2', _) = G.cut prefix suffix g2

      possibleRepl = abs . fromIntegral <$> removed
      om_init = foldl insertRep IntMap.empty [1..fromIntegral r]
      occs = G.occA g1

      testAndUpdateOne (dups,reps,oldMap) a
        | occ == 2 = (dups-1,reps-1,newMap)
        | occ == 3 = (dups+1,reps,newMap)
        | otherwise = (dups,reps,newMap)
        where (occ,newMap) = first (fromMaybe 1) $ IntMap.updateLookupWithKey (\_ x -> Just (x-1)) a oldMap
      insertRep om a = IntMap.insert (fromIntegral a) occ om
        where occ = getOcc occs a
  rev idxs inst = if hasRev @m
                     then inst & source %~ applyRop idxs
                               & numOpOfInstance +~ 1
                     else throw $ InvalidOperation "Reversion" "rev"
  trans idxs inst = if hasTrans @m
                       then inst & source %~ applyRop idxs
                                 & numOpOfInstance +~ 1
                       else throw $ InvalidOperation "Transposition" "trans"

replaceGenomes :: forall inst mod g' a'. (Genome g' a', RearrangeInstance inst, mod ~ ModelOfInstance inst, Model mod) => g' a' -> g' a' -> inst -> InstanceData mod IsStr NotExt g' a'
replaceGenomes g1 g2 _ = buildInstance @IsStr g1 g2

replaceGenomesSameType :: (ReaInstGenome inst g a) => g a -> g a -> inst -> inst
replaceGenomesSameType g1 g2 inst = 
  updateDup . updateRep $
  inst & source .~ g1
       & target .~ g2
       & lengthOfInstance .~ G.len g1

instance OnlyDup (InstanceData m IsDup e g a)

instance NoDup (InstanceData m IsPerm e g a)

instance ExtendedInstance (InstanceData m r IsExt g a)

writeInstance :: AnyInstance -> (ByteString, ByteString)
writeInstance (AnyInstance inst) = (G.toBStringOfGenes g1, G.toBStringOfGenes g2)
  where
    g1 = getSource inst
    g2 = getTarget inst

readInstance :: forall r mod g a. (Genome g a, BuildInstance r mod) => (ByteString, ByteString) -> InstanceData mod r NotExt g a
readInstance (s1,s2) = buildInstance @r g1 g2
  where
    g1 = G.fromBStringOfGenes s1
    g2 = G.fromBStringOfGenes s2

readCorrectInstance :: RepStatus -> G.GenomeType -> IsSign -> RearrangeModel -> (ByteString, ByteString) -> AnyInstance
readCorrectInstance rstatus gt sig mod gs =
    case mod of
      Rev -> withModel @MRev
      Trans -> withModel @MTrans
      TransRev -> withModel @MTransRev
    where
        withModel :: forall mod. (Model mod, FullModel mod) => AnyInstance
        withModel = 
            case rstatus of
              IsPerm -> withStatus @mod @IsPerm
              IsDup -> withStatus @mod @IsDup
              IsStr -> withStatus @mod @IsStr
        withStatus :: forall mod (r :: RepStatus). (Model mod, BuildInstance r mod, FullModel mod) => AnyInstance
        withStatus = 
            case (gt, sig) of
                (G.GSeq, Signed) -> AnyInstance (readInstance gs :: InstanceData mod r NotExt G.Gs G.SGene)
                (G.GSeq, Unsigned) -> AnyInstance (readInstance gs :: InstanceData mod r NotExt G.Gs G.UGene)
                (G.GList, Signed) -> AnyInstance (readInstance gs :: InstanceData mod r NotExt G.Gl G.SGene)
                (G.GList, Unsigned) -> AnyInstance (readInstance gs :: InstanceData mod r NotExt G.Gl G.UGene)

class (Model mod) => BuildInstance (r :: RepStatus) mod where
    buildInstance :: forall r g a. (Genome g a) => g a -> g a -> InstanceData mod r NotExt g a

instance (Model mod) => BuildInstance IsPerm mod where
    buildInstance g1 g2_
        | d /= 0 = throw $ DuplicationPresentError "BuildInstance (Perm)"
        | G.len g2 == 0 && n /= 0 = throw $ OtherError "empty g2" "BuildInstance (Perm)"
        | otherwise = buildInstance_ False g1 g2 (Just n) (Just d) (Just r)
      where
        d = G.dup g1
        r = G.rep g1
        n = G.len g1
        g2 = if G.len g2_ == 0 then G.fromListOfGenes [1..fromIntegral n] else g2_

instance (Model mod) => BuildInstance IsDup mod where
    buildInstance g1 g2
        | d /= fromIntegral r = throw $ ReplicationPresentError "BuildInstance (Dup)"
        | G.len g2 == 0 && n /= 0 = throw $ OtherError "empty g2" "BuildInstance (Dup)"
        | otherwise = buildInstance_ True g1 g2 (Just n) (Just d) (Just r)
      where
        d = G.dup g1
        r = G.rep g1
        n = G.len g1

instance (Model mod) => BuildInstance IsStr mod where
    buildInstance g1 g2
      | G.len g2 == 0 && n /= 0 = throw $ OtherError "empty g2" "BuildInstance (Str)"
      | otherwise = buildInstance_ False g1 g2 (Just n) (Just d) (Just r)
      where
        d = G.dup g1
        r = G.rep g1
        n = G.len g1

buildInstance_ :: forall mod r g a. (Genome g a, Model mod) => Bool -> g a -> g a -> Maybe Size -> Maybe Dup -> Maybe Rep -> InstanceData mod r NotExt g a
buildInstance_ isDup g1 g2 maybe_n maybe_d maybe_r =
  correct_numbered_inst
    where
      n = fromMaybe (G.len g1) maybe_n
      d = fromMaybe (G.dup g1) maybe_d
      r = fromMaybe (G.rep g1) maybe_r
      inst = InstanceData 0 g1 g2 n d r IntMap.empty
      correct_numbered_inst = inst & source %~ renumber' . G.correctNumbersMulti
                                   & target %~ renumber' . G.correctNumbersMulti
        where
          renumber' = if isDup then renumberDup d else id

makeExtended :: (Genome g a, Model mod) => InstanceData mod r NotExt g a -> InstanceData mod r IsExt g a
makeExtended inst = coerce $ inst & source %~ G.extend
                                  & target %~ G.extend
                                  & lengthOfInstance +~ 2

withoutExtencion :: (ReaInstGenome inst g a, ExtendedInstance inst) => (inst -> v) -> inst -> v
withoutExtencion f inst = f noExt
  where noExt = coerce $ inst & source %~ G.delExt
                              & target %~ G.delExt
                              & lengthOfInstance -~ 2

checkPerm :: (Genome g a, Model mod) => InstanceData mod r e g a -> Maybe (InstanceData mod IsPerm e g a)
checkPerm inst = if isPerm inst then Just $ coerce inst else Nothing

------------------- Get Info from Numbers -------------------------

duplicated :: (Gene a) => Dup -> a -> Bool
duplicated d v = abs v <= fromIntegral d

replicated :: (Gene a) => Rep -> a -> Bool
replicated r v = abs v <= fromIntegral r

singleton :: (Gene a) => Rep -> a -> Bool
singleton r = not . replicated r
