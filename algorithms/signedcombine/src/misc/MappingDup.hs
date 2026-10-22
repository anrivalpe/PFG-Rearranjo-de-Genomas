{-# LANGUAGE FlexibleInstances, TypeFamilies, FlexibleContexts, DerivingStrategies, GeneralisedNewtypeDeriving, MultiParamTypeClasses #-}

module MappingDup
  ( Perm
  , GeneratorDup
  , SepGenDup
  , MapOfDup
  , MoveDup
  , makeMapOfDup
  , bools2Map
  , makeGeneratorDup
  , getMutation
  , getCrossover
  ) where

---------------------------------------------
import Utils
import Types
---------------------------------------------
import Args (MutMethod(..), CrossMethod(..))
import Perm (Perm, permInerList, renumberToSort)
import Mapping (Gmap(..), PartialGmap(..), Move(..), GmapGenerator(..), GmapFixer(..), SepGen)
import Genome(asListOfGenes, mapGene, WhichGeneInPerm(..), getPerm, renumberDup, toEnd, len)
---------------------------------------------
import Control.Arrow (first, second)
import Control.Exception.Base (throw)
import Data.Hashable (Hashable(..))
import Data.Maybe (mapMaybe)
import Control.Monad (replicateM)
import Control.Monad.Random (MonadRandom, getRandomR, getRandoms)
import Data.Bits (Bits, bit, testBit, complementBit, zeroBits, setBit, clearBit, (.|.), xor, (.&.), complement)
import Data.Coerce (coerce)
import Data.Functor.Identity (Identity(..))
import Data.List (mapAccumR)
import System.Random.Shuffle (shuffleM)
---------------------------------------------
import qualified Data.Map as Map
---------------------------------------------
import qualified Genome as G
---------------------------------------------

newtype BIdx = BIdx Int deriving newtype (Num,Ord,Eq,Real,Enum,Integral,Show,Read)

type MapOfDup = GmapBit BitMask
type MoveDup = MoveBit BitMask
type GeneratorDup = Identity MapOfDup

type instance SepGen GeneratorDup MapOfDup = SepGenDup
data SepGenDup = SepGenDup MapOfDup BitMask BitMask Int deriving (Show)

makeMapOfDup :: Dup -> BitMask -> MapOfDup
makeMapOfDup = GmapBit

bools2Map :: (Bits b) => Dup -> [Bool] -> GmapBit b
bools2Map d bools = let bits = zipWith selectBitMask bools [0..fromIntegral d - 1] in
  GmapBit d (foldl (.|.) zeroBits bits)
  where selectBitMask bool idx = if bool then bit idx else zeroBits

makeGeneratorDup :: Dup -> GeneratorDup
makeGeneratorDup d = Identity $ GmapBit d (bit . fromIntegral $ d)

flipKBits :: (MonadRandom mr, Bits b) => Int -> GmapBit b -> mr (GmapBit b)
flipKBits k (GmapBit d m) = do
    idxs <- shuffleM [0..fromIntegral d - 1]
    let m' = foldl complementBit m . take k $ idxs
    return $ GmapBit d m'


--------------------- Aux Functions -----------------------------------

chooseKBlock :: (MonadRandom mr) => Int -> Int -> Dup -> mr (Int,BIdx)
chooseKBlock lo hi d = do
  k <- getRandomR (lo,hi)
  let (beg,end) = (0, fromIntegral d - 1)
  idx <- getRandomR (beg,end-k+1)
  return (k, fromIntegral idx)

copyBit :: (Bits b1, Bits b2) => b1 -> b2 -> Int -> b2
copyBit x y idx = if testBit x idx then setBit y idx else clearBit y idx

copySeq :: (Bits b1, Bits b2) => Int -> Int -> b1 -> b2 -> b2
copySeq b e m m' = foldl (copyBit m) m' [b..e]


--------------------- Instances ---------------------------------------

data GmapBit b = GmapBit Dup b deriving (Ord)
newtype MoveBit b = MoveBit b deriving (Show, Eq)

instance Show b => Show (GmapBit b) where
  show (GmapBit d m) = replicate (fromIntegral d - length str) '0' ++ str
    where str = show m

instance (Eq b, Bits b) => Eq (GmapBit b) where
  (==) (GmapBit d1 m1) (GmapBit d2 m2) =
    (d1 == d2) && (d1 == 0 || (
      (m1 == zeroBits && m2 == bit (fromIntegral d2)) ||
      (m1 == bit (fromIntegral d1) && m2 == zeroBits) ||
      m1 == m2))

instance Hashable b => Hashable (GmapBit b) where
  hashWithSalt salt (GmapBit _ m) = hashWithSalt salt m
  hash (GmapBit _ m) = hash m

instance Gmap MapOfDup where
    mapGenome m g = getPerm $ mapGenome' m g
    mapGenome' gm = asListOfGenes (snd . mapAccumR mapOne gm)
      where
        mapOne gm'@(GmapBit d m) a =
            let ia = fromIntegral a
             in if abs ia > fromIntegral d
                    then (gm', a)
                    else let i = abs ia - 1
                             geneValue =
                                 if testBit m i
                                     then SndValue
                                     else FstValue
                          in (GmapBit d (complementBit m i), mapGene d geneValue a)
    mapSize (GmapBit d _) = fromIntegral d
    recoverMap p g = m
      where
        m = bools2Map d . mapMaybe toBit . map snd . separateByKey $ zip lg lp
        toBit [] = Nothing
        toBit [_] = Nothing
        toBit [x, y] =
            if x < y
                then Just False
                else Just True
        toBit _ = throw (ReplicationPresentError "recoverMap (Dup)")
        lp = map abs $ permInerList p
        lg = map abs $ G.toListOfGenes g
        d = (G.dup g)
    getMutation mm lm hm =
        case mm of
            MR ->
                \m -> do
                    k <- getRandomR (lm, hm)
                    flipKBits k m
            MB ->
                \(GmapBit d m) -> do
                    (k, idx) <- second fromIntegral <$> chooseKBlock lm hm d
                    let (l, r) = (idx, idx + k - 1)
                    let m' = foldl complementBit m [l .. r]
                    return $ GmapBit d m'
    getCrossover cm lc hc =
        case cm of
            XR -> merR
            XB -> merB lc hc
            XP -> merP lc hc
            XM -> merM lc hc
      where
        merR (GmapBit d m1) (GmapBit _ m2) = do
            coins <- getRandoms
            let m' =
                    foldl
                        (\m (coin, idx) ->
                             if coin
                                 then copyBit m1 m idx
                                 else copyBit m2 m idx)
                        zeroBits $
                    zip coins [0 .. fromIntegral d - 1]
            return $ GmapBit d m'
        merB lo hi (GmapBit d m1) (GmapBit _ m2) = do
            (k, idx) <- second fromIntegral <$> chooseKBlock lo hi d
            let (l, r) = (idx, idx + k - 1)
            let m' =
                    if l == 0
                        then copySeq 0 r m2 . copySeq (r + 1) (fromIntegral d - 1) m1 $ zeroBits
                        else copySeq 0 (l - 1) m1 .
                             copySeq l r m2 . copySeq (r + 1) (fromIntegral d - 1) m1 $
                             zeroBits
            return $ GmapBit d m'
        merP lo hi (GmapBit d m1) (GmapBit _ m2) = do
            k <- getRandomR (lo, hi)
            let m' = copySeq 0 (k - 1) m1 . copySeq k (fromIntegral d - 1) m2 $ zeroBits
            return $ GmapBit d m'
        merM lo hi (GmapBit d m1) (GmapBit _ m2) = do
            k <- getRandomR (lo, hi)
            idxs <- shuffleM [0 .. fromIntegral d - 1]
            let (bs1, bs2) = splitAt k idxs
            let m' = copyMul m1 bs1 . copyMul m2 bs2 $ zeroBits
            return $ GmapBit d m'
          where
            copyMul m bs m' = foldl (copyBit m) m' bs

instance PartialGmap SepGenDup where
    type CompletGmap SepGenDup = MapOfDup
    type CompletGenerator SepGenDup = GeneratorDup

    mapParGenome (SepGenDup (GmapBit d _) band bor fixed) g =
        renumberDup (d - fromIntegral fixed) . asListOfGenes (snd . mapAccumR mapOne bor) $ g
      where
        n = len g
        mapOne bor' a =
            if abs ia > fromIntegral d || notFixed (i - 1)
                then (bor', a)
                else let i = abs ia - 1
                         geneValue =
                             if testBit bor' i
                                 then SndValue
                                 else FstValue
                      in (complementBit bor' i, toEnd n $ mapGene d geneValue a)
          where
            notFixed = testBit band
            ia = fromIntegral a
            i = abs ia - 1

    reduceGmap (SepGenDup (GmapBit d _) band bor fixed) (GmapBit d_ m) =
        if d /= d_ || any notCompatible [0 .. fromIntegral d - 1]
            then Nothing
            else Just $ GmapBit d' m'
      where
        d' = d - fromIntegral fixed
        m' = foldr copyBits zeroBits . zip [0 ..] . filter notFixed $ [0 .. fromIntegral d - 1]
        notFixed = testBit band
        copyBits (i', i) =
            if testBit m i
                then flip setBit i'
                else id
        notCompatible i = not (notFixed i) && testBit bor i /= testBit m i

    expandGmap (SepGenDup (GmapBit d _) band bor _) (GmapBit _ m) = GmapBit d m'
      where
        m' = foldr copyBits bor . zip [0 ..] . filter notFixed $ [0 .. fromIntegral d - 1]
        notFixed = testBit band
        copyBits (i, i') =
            if testBit m i
                then flip setBit i'
                else id

    reducedGenerator (SepGenDup (GmapBit d _) _ _ fixed) = makeGeneratorDup d'
        where d' = d - fromIntegral fixed


instance (Bits b) => Move (MoveBit b) (GmapBit b) where
  newtype MoveRestriction (MoveBit b) = MoveRestrictionBit b deriving (Show)

  emptyMoveRestriction _ = MoveRestrictionBit zeroBits

  getMove (GmapBit _ old) (GmapBit _ new) = MoveBit $ old `xor` new

  applyMove (MoveBit mov) (GmapBit d m) = GmapBit d $ xor mov m

  genMovesWithRestriction (MoveRestrictionBit res) (GmapBit d _) =
    coerce . filter ((==) zeroBits . (.&.) res) . map bit $ [0..fromIntegral d - 1]

  addRestriction (MoveBit mov) (MoveRestrictionBit res) = MoveRestrictionBit $ mov .|. res

  delRestriction (MoveBit mov) (MoveRestrictionBit res) = MoveRestrictionBit $ complement mov .&. res

  clearRestriction _ = MoveRestrictionBit zeroBits

instance GmapGenerator GeneratorDup MapOfDup where
  generateMaps (Identity (GmapBit d _)) c = replicateM (fromIntegral c) randMap
    where randMap = GmapBit d . flip clearBit (fromIntegral d) <$> getRandomR (bit 0, bit . fromIntegral $ d)

  standardMap = runIdentity

  combineMaps _ (GmapBit d m1) (GmapBit _ m2) = m'
    where
      p1s = map (bitToPerm m1) [0..fromIntegral d - 1]
      p2s = map (bitToPerm m2) [0..fromIntegral d - 1]
      bitToPerm m i = if testBit m i then [2 :: Int,1] else [1,2]
      m' = bools2Map d $ zipWith getBool p1s p2s
      getBool p1 p2 = let p = renumberToSort p1 p2 in
                          if p == [1,2] then False else True

  neighbors _ (GmapBit d m) = map inv1 [1..fromIntegral d]
    where inv1 idx = GmapBit d (complementBit m (idx-1))

  randomCombination _ _ [] = error patternError
  randomCombination _ create maps@(GmapBit d _:_) = replicateM create $ bools2Map d <$> mapM selectEl (zip freqs_0 freqs_1)
    where
      freqs_0 = map (getFreq maps False) [1..fromIntegral d]
      freqs_1 = map (getFreq maps True) [1..fromIntegral d]
      getFreq ms val idx = length . filter ((==) val . getBit idx) $ ms
      selectEl (w0,w1) = do
        choice <- getRandomR (0,w0+w1)
        return $ choice > w0
      getBit idx (GmapBit _ m) = testBit m (idx-1)

  flight _ p m@(GmapBit d _) = do
    k <- getK 0
    flipKBits (fromIntegral k) m
    where
      getK k = if k == d then return k else do
        val <- getRandomR (1,100)
        if val <= p then return (k+1) else getK (k+1)


instance GmapGenerator SepGenDup MapOfDup where
  generateMaps (SepGenDup (GmapBit d _) band bor fixed) c = do
      bits <- if 2 ** (fromIntegral d - fromIntegral fixed) < fromIntegral c
                 then return $ allMaps 0 zeroBits
                 else replicateM (fromIntegral c) $ getRandomR (bit 0, bit . fromIntegral $ d)
      return $ map bitToMap bits
    where
      bitToMap = GmapBit d . (.|.) bor . (.&.) band . flip clearBit (fromIntegral d)
      allMaps pos m
        | pos == fromIntegral d = [m]
        | not (testBit band pos) = allMaps (pos+1) m
        | otherwise = allMaps (pos+1) (setBit m pos) ++ allMaps (pos+1) (clearBit m pos)

  standardMap = standardMap . unwrap
  neighbors = neighbors . unwrap
  combineMaps = combineMaps . unwrap
  randomCombination = randomCombination . unwrap
  flight = flight . unwrap

instance GmapFixer SepGenDup MapOfDup where
  reset (SepGenDup std _ _ _) = SepGenDup std (complement zeroBits) zeroBits 0
  fix fixer@(SepGenDup std band bor fixed) smaps_ = if Map.null smaps_ then (fixer, smaps_) else first fixBit more_separated
    where
      smaps = Map.toAscList smaps_
      (GmapBit d _) = fst . head $ smaps
      more_separated = fst . maxWith snd . map testSeparationSingleBit . filter notFixed $ [0..fromIntegral d - 1]
      notFixed = testBit band
      testSeparationSingleBit bit_idx = (((bit_idx, val), compatible_smaps), sep)
        where
          compatible_smaps = Map.fromAscList $ if val then smaps1 else smaps0
          (sep, val)
            | null scores0 = (0, True)
            | null scores1 = (0, False)
            | otherwise = (abs (avg0 - avg1), avg0 > avg1)
          avg0 = avg scores0
          avg1 = avg scores1
          smaps_bit = (\smap@(GmapBit _ m,_) -> (smap,testBit m bit_idx)) <$> smaps
          smaps0 = map fst . filter (not . snd) $ smaps_bit
          smaps1 = map fst . filter snd $ smaps_bit
          scores0 = map snd smaps0
          scores1 = map snd smaps1
      fixBit (bit_idx,val) = SepGenDup std band' bor' (fixed + 1)
        where
          band' = band .&. complement (bit bit_idx)
          bor' = if val then bor .|. bit bit_idx else bor

instance Wrapped SepGenDup where
    type Unwrapped SepGenDup = GeneratorDup
    wrap (Identity m) = SepGenDup m (complement zeroBits) zeroBits 0
    unwrap (SepGenDup m _ _ _) = Identity m
