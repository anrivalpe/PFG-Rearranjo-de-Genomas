{-# LANGUAGE ScopedTypeVariables, TupleSections, FlexibleContexts, DataKinds, KindSignatures, FlexibleInstances, TypeApplications, AllowAmbiguousTypes, TypeFamilies, DeriveFoldable #-}

module Aux
  ( Rseq
  , AnyOp(..)
  , Rop(..)
  , RevOp(..)
  , TransOp(..)
  , rearrange
  , Model(..)
  , MRev
  , MTrans
  , MTransRev
  , Assigments
  , Ass
  , emptyAssigmets
  , recordChanges
  , applyAss
  , PairMaches
  , PairMacheEntry
  , Due(..)
  , toDue
  , DueMap(..)
  , canonicOri
  , invOri
  , toDueMap
  , getPairMaches
  , pmsList
  , pmsLength
  , pmsFind
  , dueMapFind
  , dueMapAlter
  , dueMapFindDuesWithGene
  , isConflict
  , StripMap
  , stripsToMap
  , stmEmpty
  , stmLookup
  , stmOrientedLookup
  , stmInsert
  , stmLookupInsert
  , BreakPoints
  , Strip(fromStrip)
  , toStrip
  , Strips
  , contractedRepresentation'
  , strips2Breaks
  , breaks2Strips
  , break2Strips
  , permutationBreaks
  ) where

-------------------------------------------------
import Types
import Utils
-------------------------------------------------
import Genome (Gene(..), Genome, asListOfGenes, renumber, correctNumbersMulti)
import Perm (renumberToSort)
-------------------------------------------------
import Control.Arrow (first, second, (***))
import Data.Array (Array, (!), accumArray, Ix)
import Data.Coerce (coerce)
import Data.IntMap (IntMap)
import Data.Maybe (fromMaybe)
import Data.Traversable (mapAccumL)
import GHC.Exts (toList, IsList, Item)
-------------------------------------------------
import qualified Data.IntMap as IntMap
import qualified Data.List as List
-------------------------------------------------
import qualified Genome as G
--------------------------------------------------

--------------- Sequence of Rearranges -----------------------

class (Show rop) => Rop rop where
  applyRop :: (Genome g a) => rop -> g a -> g a
  translateRop :: Idx -> rop -> rop
  mirrorRop :: Size -> rop -> rop

type Rseq = [AnyOp]
data RevOp = R Idx Idx deriving (Show)
data TransOp = T Idx Idx Idx deriving (Show)
data AnyOp = Op1 RevOp | Op2 TransOp

instance Show AnyOp where
  show (Op1 op) = show op
  show (Op2 op) = show op

instance Rop RevOp where
  applyRop (R i j) = G.rev i j
  translateRop t (R i j) = R (t + i) (t + j)
  mirrorRop n (R i j) = R (fromIntegral n - j + 1) (fromIntegral n - i + 1)

instance Rop TransOp where
  applyRop (T i j k) = G.trans i j k
  translateRop t (T i j k) = T (t + i) (t + j) (t + k)
  mirrorRop n (T i j k) = let n' = fromIntegral n in T (n' - k + 1) (n' - j + 1) (n' - i + 1)

instance Rop AnyOp where
  applyRop (Op1 op) g = applyRop op g
  applyRop (Op2 op) g = applyRop op g
  translateRop t (Op1 op) = Op1 $ translateRop t op
  translateRop t (Op2 op) = Op2 $ translateRop t op
  mirrorRop n (Op1 op) = Op1 $ mirrorRop n op
  mirrorRop n (Op2 op) = Op2 $ mirrorRop n op

rearrange :: (Genome g a, Rop rop) => [rop] -> g a -> g a
rearrange = flip $ List.foldl' (flip applyRop)

-------------------------- Models ------------------------------

class Model mod where
    type ContractedGene mod
    model :: forall mod. RearrangeModel
    hasRev :: forall mod. Bool
    hasTrans :: forall mod. Bool

data MRev
instance Model MRev where
    type ContractedGene MRev = G.SGene
    model = Rev
    hasRev = True
    hasTrans = False

data MTrans
instance Model MTrans where
    type ContractedGene MTrans = G.UGene
    model = Trans
    hasRev = False
    hasTrans = True

data MTransRev
instance Model MTransRev where
    type ContractedGene MTransRev = G.SGene
    model = TransRev
    hasRev = True
    hasTrans = True

-------------------------- Assigment ------------------------------

newtype Assigments a = Assigments (IntMap a)
type Ass a = (Idx,a)

emptyAssigmets :: Assigments a
emptyAssigmets = Assigments IntMap.empty

setAss :: Assigments a -> Ass a -> Assigments a
setAss (Assigments ass) (idx,a) = Assigments $ IntMap.insert (fromIntegral idx) a ass

getAss :: Assigments a -> Idx -> Maybe a
getAss (Assigments ass) idx = IntMap.lookup (fromIntegral idx) ass

recordChanges :: Assigments a -> [Ass a] -> Assigments a
recordChanges = foldl setAss

applyAss :: (Genome g a) => Assigments a -> g a -> g a
applyAss ass = renumber . correctNumbersMulti . asListOfGenes (zipWith assign [1..])
  where
    assign i a = fromMaybe a (getAss ass i)

-------------------------- Due ------------------------------

data Due mod (a :: *) = Due a a deriving (Eq, Ord, Show)

toDue :: forall mod a. (a,a) -> Due mod a
toDue (a,b) = Due a b

instance forall mod a. (Model mod, Gene a) => Orientable (Due mod a) where
    getOri due =
        case model @mod of
          Trans -> LR
          Rev -> getOri' due
          TransRev -> getOri' due
      where
          getOri' (Due a1 a2)
            | a1 < 0 && a2 < 0 = RL
            | a1 > 0 && a2 < 0 = if abs a1 < abs a2 then LR else RL
            | a1 < 0 && a2 > 0 = if abs a1 < abs a2 then LR else RL
            | otherwise = if haveOri a1 || abs a1 < abs a2 then LR else RL
    invOri (Due a1 a2) =
        case model @mod of
          Trans -> Due a1 a2
          Rev -> Due (invOri a2) (invOri a1)
          TransRev -> Due (invOri a2) (invOri a1)

searchOrientation :: (Gene a, Model mod) => Ori -> Due mod a -> Due mod a
searchOrientation ori = (\due@(Due a1 a2) -> case ori of {LR -> due; RL -> (Due a2 a1)}) . canonicOri

newtype DueArray a e = DueArray (Array a [(a,e)]) deriving (Show)

toDueArray :: forall a mod e. (Gene a, Model mod) => Ori -> Size -> [(Due mod a, e)] -> DueArray a e
toDueArray ori n = DueArray . accumArray (++) [] (beg, fromIntegral n) . map indexWithOriDue
  where
    indexWithOriDue (due,e) =
      let (Due a1 a2) = canonicOri due in
      case ori of
        LR -> (a1,[(a2,e)])
        RL -> (a2,[(a1,e)])
    beg = fromIntegral $ min 1 (invOri . fromIntegral $ n :: a)

dueArrayFind :: (Gene a, Model mod) => DueArray a e -> Due mod a -> [e]
dueArrayFind (DueArray dueArray) due = map snd . filter ((==) a2 . fst) $ (dueArray ! a1)
  where (Due a1 a2) = canonicOri due

dueArrayListOneGene :: (Ix a) => DueArray a e -> a -> [e]
dueArrayListOneGene (DueArray dueArray) a = map snd $ dueArray ! a

instance Foldable (DueArray a) where
  foldr fun acc (DueArray dueArray) = foldr fun' acc dueArray
    where fun' list acc' = foldr fun acc' $ map snd list

  foldMap fun (DueArray dueArray) = foldMap fun' dueArray
    where fun' pmList = foldMap fun $ map snd pmList

data DueMap mod (e :: *) = DueMap Ori (IntMap (IntMap e)) deriving (Show)

invSearchOrientation :: Ori -> Due mod a -> Due mod a
invSearchOrientation ori due@(Due a1 a2) = case ori of {LR -> due; RL -> Due a2 a1}

toDueMap :: (Gene a, Model mod) => Ori -> (e -> e -> e) -> [(Due mod a,e)] -> DueMap mod e
toDueMap ori combiFun = DueMap ori . IntMap.fromListWithKey combiFun' . map (toMap . adjustSOri)
  where
    combiFun' _ = IntMap.unionWith combiFun
    toMap (Due a1 a2,e) = (fromIntegral a1, IntMap.singleton (fromIntegral a2) e)
    adjustSOri = first (searchOrientation ori)

dueMapFind :: (Gene a, Model mod) => DueMap mod e -> Due mod a -> Maybe e
dueMapFind (DueMap ori dueMap) due =
    IntMap.lookup (fromIntegral a2) =<< IntMap.lookup (fromIntegral a1) dueMap
  where (Due a1 a2) = searchOrientation ori due

dueMapAlter :: (Gene a, Model mod) => (Maybe e -> e) -> Due mod a -> DueMap mod e -> DueMap mod e
dueMapAlter alterFun due (DueMap ori dueMap) = DueMap ori $ IntMap.alter alterFun' (fromIntegral a1) dueMap
  where
    alterFun' Nothing = Just $ IntMap.singleton (fromIntegral a2) (alterFun Nothing)
    alterFun' (Just map2) = Just $ IntMap.alter (Just . alterFun) (fromIntegral a2) map2
    (Due a1 a2) = searchOrientation ori due

dueMapFindDuesWithGene :: (Gene a) => DueMap mod e -> a -> (e -> Bool) -> [Due mod a]
dueMapFindDuesWithGene (DueMap ori dueMap) a filterFun = geneDues
  where
    geneDues = map (invSearchOrientation ori . Due a) correctGenes
    correctGenes = map (fromIntegral . fst) . filter (filterFun . snd) . IntMap.toList $ geneMap
    geneMap = fromMaybe IntMap.empty . IntMap.lookup (fromIntegral a) $ dueMap

-------------------------- Pair Mach ------------------------------

type PairMaches mod a = PairMaches' a (PairMacheEntry mod a)
data PairMaches' a e = PairMaches (DueArray a e) Size deriving (Show)
type PairMacheEntry mod a = (Due mod a, Due mod a, Idx, Idx, Id)
type Dues mod a = DueArray a (DuesEntry mod a)
type DuesEntry mod a = (Idx, Due mod a)
type Id = Int

isConflict :: (Gene a, Model mod) => PairMacheEntry mod a -> PairMacheEntry mod a -> Bool
isConflict pm@(due1,due2,idx1,idx2,_) pm'@(_,_,idx1',idx2',_) =
  if idx1 > idx1'
  then isConflict pm' pm
  else idx1 == idx1' && (idx2 /= idx2') ||
       idx1+1 < idx1' && (List.intersect [idx2,idx2+1] [idx2',idx2'+1] /= []) ||
       idx1+1 == idx1' && not ( idx2 + 1 == idx2' && due1 == due2 ||
                            idx2 == idx2' + 1 && due1 == invDue due2 )
                                where invDue (Due a1 a2) = Due (invOri a2) (invOri a1)

getPairMaches :: forall a mod. (Gene a, Model mod) => Ori -> [a] -> [a] -> Size -> PairMaches mod a
getPairMaches ori l1 l2 n = PairMaches pms (Size last_id - 1)
  where

    (last_id, pms) = second (toDueArray ori (fromIntegral n) . concat) $ mapAccumL checkPair 1 dues2l

    dues1 = toDues . zip [1..] . map (\(a1,a2) -> Due a1 a2) $ lPairs l1
    dues2l = zip [1..] . map (\(a1,a2) -> Due a1 a2) $ lPairs l2

    toDues :: [(Idx,Due mod a)] -> Dues mod a
    toDues = toDueArray LR (fromIntegral n) . map (\(idx,due) -> (due,(idx,due)))

    checkPair :: Id -> (Idx,Due mod a) -> (Id, [(Due mod a, PairMacheEntry mod a)])
    checkPair ind (idx2,due2) = mapAccumL toEntry ind dues1l
      where
        dues1l = dueArrayFind dues1 due2
        toEntry :: Id -> (Idx,Due mod a) -> (Id, (Due mod a, PairMacheEntry mod a))
        toEntry i (idx1,due1) = (i+1, (due1, (due1,due2,idx1,idx2,i)))

pmsList :: (Gene a) => PairMaches mod a -> a -> [PairMacheEntry mod a]
pmsList (PairMaches pms _) = dueArrayListOneGene pms

pmsFind :: (Gene a, Model mod) => PairMaches mod a -> Due mod a -> [PairMacheEntry mod a]
pmsFind (PairMaches pms _) = dueArrayFind pms

pmsLength :: PairMaches mod a -> Size
pmsLength (PairMaches _ n) = n

instance (Gene a) => Foldable (PairMaches' a) where
  foldr fun acc (PairMaches pms _) = foldr fun acc pms
  foldMap fun (PairMaches pms _) = foldMap fun pms

--------------------- Strips/Break Points -------------------------------

newtype StripMap mod a v = STM (IntMap [(Strip mod a,v)])
data Strip mod (a :: *) = Strip {fromStrip :: [a]} deriving (Eq,Ord,Foldable,Show)
type BreakPoints = ([Idx],[Idx])
type Strips mod a = ([Strip mod a],[Strip mod a])

toStrip :: forall mod l. (IsList l) => l -> Strip mod (Item l)
toStrip = Strip . toList

instance forall a mod. (Orientable a, Ord a, Model mod) => Orientable (Strip mod a) where
    getOri strip =
        case model @mod of
          Trans -> LR
          Rev -> getOri' strip
          TransRev -> getOri' strip
      where
          getOri' s = let rs = invOri s in if s >= rs then LR else RL
    invOri (Strip s) =
        case model @mod of
          Trans -> Strip s
          Rev -> Strip . map invOri . reverse $ s
          TransRev -> Strip . map invOri . reverse $ s

stmEmpty :: StripMap mod a v
stmEmpty = STM IntMap.empty

-- return result with orientation
stmOrientedLookup :: (Gene a, Gene v, Model mod) => Strip mod a -> StripMap mod a v -> Maybe v
stmOrientedLookup s stm = (\a -> orient ori a) <$> stmLookup_ s' k stm
  where
    ori = getOri s
    s' = canonicOri s
    k = fromIntegral . head . fromStrip $ s'

stmLookup :: (Gene a, Model mod) => Strip mod a -> StripMap mod a v -> Maybe v
stmLookup s stm = stmLookup_ s' k stm
  where
    s' = canonicOri s
    k = fromIntegral . head . fromStrip $ s'

stmLookup_ :: (Gene a) => Strip mod a -> Int -> StripMap mod a v -> Maybe v
stmLookup_ s' k (STM m) = fmap snd $ IntMap.lookup k m >>= List.find ((==) s' . fst)

stmInsert :: (Gene a, Model mod) => Strip mod a -> v -> StripMap mod a v -> StripMap mod a v
stmInsert s val stm = stmInsert_ s' k val stm
  where
    s' = canonicOri s
    k = fromIntegral . head . fromStrip $ s'

stmInsert_ :: (Gene a) => Strip mod a -> Int -> v -> StripMap mod a v -> StripMap mod a v
stmInsert_ s' k val (STM m) = STM m'
  where
    m' = case IntMap.lookup k m of
           Nothing -> IntMap.insert k [(s',val)] m
           Just _ -> IntMap.adjust ([(s',val)]++) k m

-- if not found (return False and insert) else (return True)
stmLookupInsert :: (Gene a, Model mod) => Strip mod a -> v -> StripMap mod a v -> (StripMap mod a v, v, Bool)
stmLookupInsert s val stm = 
  case stmLookup_ s' k stm of
    Nothing -> (stmInsert_ s' k val stm, val, False)
    Just v -> (stm, v, True)
  where
    s' = canonicOri s
    k = fromIntegral . head . fromStrip $ s'

permutationBreaks :: forall mod a. (Model mod, Gene a) => [a] -> [a] -> BreakPoints
permutationBreaks l1 l2 = (bp1,bp2)
    where
        l = renumberToSort l1 l2
        strips = reverse $ makeStrip l [] []
        bp1 = strips2Break . fmap (toStrip @mod) $ strips
        bp2 = init . List.sort . fmap (maximum . map (abs . fromIntegral)) $ strips

        makeStrip [] acc s = if null s then acc else (reverse s:acc)
        makeStrip [x] acc s = (reverse (x:s):acc)
        makeStrip (x:y:xs) acc s =
            if isPermBreak (model @mod) x y
               then makeStrip (y:xs) (reverse (x:s):acc) []
               else makeStrip (y:xs) acc (x:s)

isPermBreak :: (Gene a) => RearrangeModel -> a -> a -> Bool
isPermBreak model x y =
    if haveOri x then y - x /= 1 else
    case model of
      Rev -> abs (fromIntegral y - fromIntegral x) /= (1 :: Int)
      TransRev -> abs (fromIntegral y - fromIntegral x) /= (1 :: Int)
      Trans -> (fromIntegral y - fromIntegral x) /= (1 :: Int)

strips2Breaks :: Strips mod a -> BreakPoints
strips2Breaks (s1,s2) = (strips2Break s1, strips2Break s2)

strips2Break :: [Strip mod a] -> [Idx]
strips2Break [] = []
strips2Break s = init . List.scanl1 (+) . coerce . map (length . fromStrip) $ s

breaks2Strips :: forall mod a. [a] -> [a] -> BreakPoints -> Strips mod a
breaks2Strips l1 l2 bps =
  (break2Strips (fst bps) l1, break2Strips (snd bps) l2)

break2Strips :: forall mod a. [Idx] -> [a] -> [Strip mod a]
break2Strips bs_ = let bs = coerce bs_ in map Strip . takes (zipWith (-) bs (0:bs))
  where
    takes _ [] = []
    takes [] l = [l]
    takes (n:ns) l = take n l : takes ns (drop n l)

strips2contRep :: forall a mod. (Gene a, Model mod) => Strips mod a -> Size -> StripMap mod a Int -> Bool -> ([Int], [Int])
strips2contRep (strip1, strip2) n m asPerm = (strips2genome strip1, strips2genome strip2)
  where
    strips2genome = snd . List.mapAccumL mapStrip IntMap.empty
    mapStrip :: IntMap Int -> Strip mod a -> (IntMap Int, Int)
    mapStrip repeated s = (repeated', a')
      where
        incrementRepetition Nothing = Just 1
        incrementRepetition (Just x) = Just $ x + 1
        ori = getOri s
        a' = orient ori a
        (a,repeated') = case stmLookup s m of
                  Nothing -> error "Error on contractedRepresentation."
                  Just v ->
                      if asPerm
                         then (,IntMap.alter incrementRepetition (abs v) repeated) $
                             case IntMap.lookup (abs v) repeated of
                               Nothing -> v
                               Just x -> v + fromIntegral n * x
                         else (v,repeated)

stripsToMap :: forall a a' mod. (Gene a, Gene a', Model mod) => a' -> Strips mod a -> (StripMap mod a a', a')
stripsToMap ini_a = foldl addStrip (stmEmpty, ini_a) . snd
  where
    addStrip :: (StripMap mod a a', a') -> Strip mod a -> (StripMap mod a a', a')
    addStrip (m,count) s = (m', count')
      where
        count' = if keepOld then count else count+1
        (m', _, keepOld) = stmLookupInsert s count m

contractedRepresentation' :: forall mod a a'. (Gene a, Gene a', a' ~ ContractedGene mod, Model mod) => [a] -> [a] -> Size -> BreakPoints -> Bool -> ([a'], [a'])
contractedRepresentation' l1 l2 n bps asPerm =
  let strips = breaks2Strips @mod l1 l2 bps
      m = fst $ stripsToMap 1 strips in
  (map fromIntegral *** map fromIntegral) $ strips2contRep strips n m asPerm
