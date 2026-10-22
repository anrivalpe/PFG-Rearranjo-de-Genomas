{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, GeneralisedNewtypeDeriving, DerivingStrategies, TypeFamilies, FlexibleContexts #-}

module Genome
  ( Genome(..)
  , Gene(..)
  , Positions
  , PositionsOri
  , Occurences
  , asListOfGenes
  , getPos
  , getPosOri
  , orientGene
  , randomGenome
  , shuffleGenome
  , randomGenomeWithDuplicates
  , randomGenomeWithReplicas
  , mapGene
  , getPerm
  , getOcc
  , getOccList
  , occGrt
  , renumber
  , renumberDup
  , correctNumbersMulti
  , WhichGeneInPerm(..)
  , GenomeType(..)
  , UGl
  , SGl
  , UGs
  , SGs
  , Gl
  , Gs
  , UGene
  , SGene
  , toEnd
  , toEndK
  ) where

----------------------------------------------------
import Types
import Utils
import Perm (Perm, toPerm)
import ArgsDB (DBType(..))
----------------------------------------------------
import Control.Arrow ((***))
import Control.Exception.Base (throw)
import Control.Monad.Random (getRandoms, getRandomRs, MonadRandom)
import Data.Array (Array, bounds)
import Data.Foldable (toList)
import Data.Functor.Classes (Eq1, Show1, showsPrec1, eq1)
import Data.Ix (Ix)
import Data.Maybe (fromJust, fromMaybe, listToMaybe)
import Data.Sequence (Seq(..))
import Data.Set (Set)
import Numeric.Natural (Natural)
import System.Random.Shuffle (shuffleM)
----------------------------------------------------
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.IntMap as IntMap
import qualified Data.List as List
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Array as Array
----------------------------------------------------

data GenomeType = GSeq | GList
type SGene = Int
type UGene = Natural
type Gs = ListishGenome Seq
type Gl = ListishGenome []
type SGl = Gl SGene
type UGl = Gl UGene
type SGs = Gs SGene
type UGs = Gs UGene

newtype Alphabet a = Alphabet (Set a) deriving newtype (Foldable,Eq)
newtype Positions a = Positions (Array a [Idx]) deriving newtype (Eq)
newtype PositionsOri a = PositionsOri (Array a [Idx]) deriving newtype (Eq)
newtype Occurences a = Occurences (Array a Occ) deriving newtype (Eq)

class (Integral a, Num a, Ix a, Read a, Show a, Orientable a) => Gene a where
  haveOri :: a -> Bool -- whether the orientation is know

class (Show (g a), Eq (g a), Gene a, Functor g) => Genome g a where
  fromListOfGenes :: [a] -> g a -- convert list of genes to Genome
  toListOfGenes :: g a -> [a] -- convert Genome to list of genes
  fromStringOfGenes :: String -> g a -- convert string to Genome
  toStringOfGenes :: g a -> String -- convert Genome to string
  fromBStringOfGenes :: BS.ByteString -> g a -- convert ByteString to Genome
  toBStringOfGenes :: g a -> BS.ByteString -- convert Genome to ByteString

  rev :: Idx -> Idx -> g a -> g a -- reversion event
  trans :: Idx -> Idx -> Idx -> g a -> g a -- transposition event
  cut :: PrefixSize -> SuffixSize -> g a -> (g a,[a]) -- cuts extremities of Genome, return list of elements removed
  mirror :: g a -> g a -- genome with all the genes mirror in the center, does not affect signs
  extend :: g a -> g a -- add extra genes on the extremities of the genome
  delExt :: g a -> g a -- remove extra genes on the extremities of the genome

  alf :: g a -> Alphabet a -- the alphabet
  len :: g a -> Size -- the size
  sin :: g a -> Int -- the number of singleton genes
  dup :: g a -> Dup -- the number of duplicated genes
  rep :: g a -> Rep -- the number of replicated genes
  beg :: g a -> a -- the first gene
  occ :: g a -> a -> Occ -- the number of occurrences of a gene
  occA :: g a -> Occurences a -- the occurrence of all genes
  pos :: g a -> a -> [Idx] -- the positions of a gene
  posA :: g a -> Positions a -- the positions of all genes
  posAS :: g a -> PositionsOri a -- same as Pos but only find genes with the same orientation
  ele :: g a -> Idx -> a -- the gene in the given index
  sig :: g a -> Bool -- whether the orientation is know

  commumPrefix :: g a -> g a -> PrefixSize -- common prefix between genomes
  commumSuffix :: g a -> g a -> SuffixSize -- common suffix between genomes

  fromStringOfGenes = fromListOfGenes . map read . words
  toStringOfGenes = unwords . map show . toListOfGenes
  fromBStringOfGenes = fromListOfGenes . map readG . BS.words
  toBStringOfGenes = BS.pack . toStringOfGenes
  trans i j k = rev i (k-1) . rev i (j-1) . rev j (k-1)
  sin g = length . filter (== 1) $ occ g . fromIntegral <$> [1..(len g)]
  dup g = Dup . length . filter (== 2) $ occ g . fromIntegral <$> [1..(len g)]
  rep g = Rep . length . filter (> 1) $ occ g . fromIntegral <$> [1..(len g)]
  occ g = fromIntegral . length . pos g
  posA g = Positions . Array.array (0, fromIntegral $ len g) . fmap (\a -> (a, pos g a)) . toList . alf $ g
  occA g = Occurences . Array.array (0, fromIntegral $ len g) . fmap (\a -> (a, occ g a)) . toList . alf $ g
  commumSuffix = curry $ uncurry commumPrefix . (mirror *** mirror)

asListOfGenes :: (Genome g a) => ([a] -> [a]) -> g a -> g a
asListOfGenes f = fromListOfGenes . f . toListOfGenes

orientGene :: (Gene a) => a -> a -> a
-- copy the orientation of a to b
orientGene a b = orient (getOri a) b

getPos :: (Gene a) => Positions a -> a -> [Idx]
-- read values of Positions
getPos (Positions array) a_ =
  if a < start || a > end
  then throw $ IndexError (fromIntegral a) "getPos"
  else array Array.! a
  where
    (start,end) = bounds array
    a = abs a_

getPosOri :: (Gene a) => PositionsOri a -> a -> [Idx]
-- read values of Positions
getPosOri (PositionsOri array) a =
  if a < start || a > end
  then throw $ IndexError (fromIntegral a) "getPos"
  else array Array.! a
  where (start,end) = bounds array

getOcc :: (Gene a) => Occurences a -> a -> Occ
-- read values of Occurences
getOcc (Occurences array) a_ =
  if a < start || a > end
  then throw $ IndexError (fromIntegral a) "getOcc"
  else array Array.! a
  where
    (start,end) = bounds array
    a = abs a_

getOccList :: Occurences a -> [Occ]
-- get list of occurrence, in increasing order of gene values
getOccList (Occurences array) = filter (/= 0) . Array.elems $ array

getPerm :: (Genome g a) => g a -> Perm
-- get some permutation correspondent to the genome (replicas are mapped arbitrarily)
getPerm g = toPerm (sig g) . map fromIntegral . toListOfGenes . renumber $ g

renumber :: (Genome g a) => g a -> g a
-- renumber to use numbers from 1 to size of alphabet
renumber = asListOfGenes renumberList
  where renumberList l =
          let m = IntMap.fromList $ zip (uniqueSort $ fromIntegral . abs <$> l) [1..] in renumberL l m

renumberL :: (Gene a) => [a] -> IntMap.IntMap a -> [a]
renumberL l m = map (\x -> let el = m IntMap.! (fromIntegral . abs $ x) in orientGene x el) l

occGrt :: (Genome g a) => g a -> Occ
-- the greatest occurrence of any gene
occGrt g = let (Occurences array) = occA g in maximum array

toEnd :: (Gene a) => Size -> a -> a
toEnd = toEndK 1

toEndK :: (Gene a) => Int -> Size -> a -> a
toEndK k n a = if a < 0 then a - dis else a + dis
  where dis = fromIntegral k * fromIntegral n

---------------------- Specific for Duplicated Genes ----------------------------

randomGenomeWithDuplicates :: (Genome g a, MonadRandom mon) => Size -> Dup -> DBType -> Bool -> mon (g a)
-- Randomly generate a genome with a given number of duplicates
randomGenomeWithDuplicates n_ d_ dbtype isSecond = do
  let list = [1..d] ++ [1..d] ++ [(2*d + 1)..n]
  let hardList = (++ [(n-remening_n+1)..n]) . concatMap addSeparation <$> shuffleM [1..hard_n]
  coins <- getRandoms
  let swaps b v = if b then v else invOri v
  fromListOfGenes <$> case dbtype of
    NormalDB -> zipWith swaps coins <$> shuffleM list
    HardDB -> zipWith swaps coins <$> hardList
  where
    n = fromIntegral n_
    hard_n = n `div` 3
    remening_n = n `mod` 3
    d = fromIntegral d_
    addSeparation a =
      if isSecond
      then [(hard_n - a + 1) + hard_n, a', (hard_n - a + 1) + 2*hard_n]
      else [a + hard_n, a', a + 2*hard_n]
      where a' = if a <= 2*d then (a+1) `div` 2 else a

data WhichGeneInPerm = FstValue | SndValue

mapGene :: (Gene a) => Dup -> WhichGeneInPerm -> a -> a
-- Map gene in one of the two possible values
mapGene _ FstValue a = orientGene a (abs a)
mapGene d SndValue a = orientGene a (fromIntegral d + abs a)

renumberDup :: (Genome g a) => Dup -> g a -> g a
-- Renumber to use number from 1 to size of alphabet, leave gap to map the replicas, assumes that replicas are on the begging
renumberDup d = asListOfGenes renumberList
  where
    renumberList l =
      let int_d = fromIntegral d
          m = IntMap.fromList $ zip (uniqueSort $ fromIntegral . abs <$> l) ([1..int_d]++[2*int_d + 1..]) in
          renumberL l m

correctNumbersMulti :: (Genome g a) => g a -> g a
correctNumbersMulti g = renumber $ fmap (correctNumberMulti g) g
  where
    correctNumberMulti g a
      | getOcc occs a > 2 = toEnd n a
      | getOcc occs a == 2 = a
      | otherwise = toEnd n . toEnd n $ a
      where
          n = len g
          occs = occA g


---------------------- Generate Random Genome ----------------------------

randomGenome :: (Genome g a, MonadRandom mon) => Size -> Int -> mon (g a)
randomGenome n_ last_value = fromListOfGenes . take n . map fromIntegral <$> getRandomRs (1,last_value)
  where n = fromIntegral n_

shuffleGenome :: (Genome g a, MonadRandom mon) => g a -> mon (g a)
shuffleGenome g = do
    coins <- getRandoms
    fmap (fromListOfGenes . zipWith swaps coins) . shuffleM . toListOfGenes $ g
    where swaps b v = if b then v else invOri v

randomGenomeWithReplicas :: (Genome g a, MonadRandom mon) => Size -> Rep -> Int -> Int -> Bool -> DBType -> mon (g a)
-- Randomly generate two genomes with number of replicated genes between low and high
randomGenomeWithReplicas n_ d_ low high isSecond dbtype = do
  reps <- getRandomRs (fromIntegral low, fromIntegral high)
  let list = zip [1..] . take (fromIntegral rep_n) . concat . zipWith replicate reps $ [1..rep_n]
  toGenome list
  where
    n = fromIntegral n_
    d = fromIntegral d_
    rep_n = case dbtype of {NormalDB -> d; HardDB -> n `div` 3}
    remening_n = n `mod` 3
    toGenome l = do
      l' <- case dbtype of
            NormalDB -> shuffleM $ map snd l ++ [d+1..n]
            HardDB -> (++ [(n-remening_n+1)..n]) . concatMap (addSeparation isSecond) <$> shuffleM l
      coins <- getRandoms
      return . fromListOfGenes $ zipWith swaps coins l'
    swaps b v = if b then v else invOri v
    addSeparation isSecond (i,a) =
      if isSecond
      then [(rep_n - i + 1) + rep_n, a, (rep_n - i + 1) + 2*rep_n]
      else [i + rep_n, a, i + 2*rep_n]

---------------------- Gene/Genome Instances ----------------------------

instance Gene Int where
  haveOri _ = True

instance Gene Natural where
  haveOri _ = False

newtype ListishGenome l a = ListishGenome (l a) deriving newtype (Foldable, Functor, Eq1, Show1, Listish, Semigroup)

instance (Show1 l, Show a) => Show (ListishGenome l a) where
  showsPrec = showsPrec1
instance (Eq1 l, Eq a) => Eq (ListishGenome l a) where
  (==) = eq1

instance (Listish l, Gene a, Show (l a), Eq (l a), Semigroup (l a)) => Genome (ListishGenome l) a  where
  toListOfGenes = toList
  fromListOfGenes = fromList'
  rev i_ j_ l = start <> (fmap invOri . reverse' $ mid) <> end
    where
      (start,rest) = splitAt' i l
      (mid,end) = splitAt' (j - i + 1) rest
      i = fromIntegral i_ - 1
      j = fromIntegral j_ - 1

  trans i_ j_ k_ l = start <> mid2 <> mid1 <> end
    where
      (start,rest1) = splitAt' i l
      (mid1,rest2) = splitAt' (j - i) rest1
      (mid2,end) = splitAt' (k - j) rest2
      i = fromIntegral i_ - 1
      j = fromIntegral j_ - 1
      k = fromIntegral k_ - 1

  cut prefix suffix l = (mid, toList $ if middle > 0 then start <> end else start)
    where
      middle = length l - prefix - suffix
      (start,rest) = splitAt' prefix l
      (mid,end) = splitAt' middle rest

  mirror = reverse'

  extend l = if null l
    then fromList' [0,1]
    else fromList' [0] <> l <> fromList' [1 + (maximum . fmap abs $ l)]
  delExt = delExt'

  alf = Alphabet . Set.fromList . map abs . toList
  len = Size . length
  sin = length . filter (\(_,o) -> o == 1) . countReplicas . map abs . toList
  dup = Dup . length . filter (\(_,o) -> o == 2) . countReplicas . map abs . toList
  rep = Rep . length . filter (\(_,o) -> o > 1) . countReplicas . map abs . toList
  pos l a = map (Idx . (+) 1) . findIndices' (\x -> abs x == abs a) $ l
  posA l = Positions . Array.accumArray (++) [] (0,maxAbs) . zipWith toArrayEl [1..] . toList $ l
    where
      maxAbs = max (maximum l) (abs . minimum $ l)
      toArrayEl i a = (abs a,[Idx i])
  posAS l = if sig l
            then PositionsOri . Array.accumArray (++) [] (-maxAbs,maxAbs) . zipWith toArrayEl [1..] . toList $ l
            else let (Positions arr) = posA l in PositionsOri arr
    where
      maxAbs = max (maximum l) (abs . minimum $ l)
      toArrayEl i a = (a,[Idx i])
  occA l = Occurences . Array.accumArray (+) 0 (0,maxAbs) . map toArrayEl . toList $ l
    where
      maxAbs = if null l then 0 else max (maximum l) (abs . minimum $ l)
      toArrayEl a = (abs a,1)
  ele l i = fromMaybe (throw $ IndexError i "ele") $ getIndex' l (fromIntegral i - 1)
  beg = head'
  sig l = haveOri proxy
    where proxy = 0; _ = fromList' [proxy] <> l

  commumPrefix l1 = length . takeWhileL' id . zipWith (==) (toList l1) . toList
  commumSuffix l1 = length . takeWhileR' id . zipWith (==) (toList l1) . toList

readG :: (Gene a) => BS.ByteString -> a
readG = fromIntegral . fst . fromJust . BS.readInt

---------------------- Listish ----------------------------

class (Foldable l, Functor l, Eq1 l, Show1 l) => Listish l where
  fromList' :: [a] -> l a
  splitAt' :: Int -> l a -> (l a, l a)
  reverse' :: l a -> l a
  head' :: l a -> a
  tail' :: l a -> l a
  findIndices' :: (a -> Bool) -> l a -> [Int]
  takeWhileL' :: (a -> Bool) -> l a -> l a
  takeWhileR' :: (a -> Bool) -> l a -> l a
  getIndex' :: l a -> Int -> Maybe a
  delExt' :: l a -> l a

instance Listish [] where
  fromList' = id
  splitAt' = splitAt
  reverse' = reverse
  head' = head
  tail' = tail
  findIndices' = List.findIndices
  takeWhileL' = takeWhile
  takeWhileR' f = takeWhile f . reverse
  getIndex' l i = listToMaybe . drop i $ l
  delExt' = init . tail

instance Listish Seq where
  fromList' = Seq.fromList
  splitAt' = Seq.splitAt
  reverse' = Seq.reverse
  head' = flip Seq.index 0
  tail' l = let (_:<|t ) = l in t
  findIndices' = Seq.findIndicesL
  takeWhileL' = Seq.takeWhileL
  takeWhileR' = Seq.takeWhileR
  getIndex' l i = Seq.lookup i l
  delExt' s = let ((_:<|s'):|>_) = s in s'
