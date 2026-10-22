{-# LANGUAGE TupleSections, FlexibleInstances, MultiParamTypeClasses, RecordWildCards, TypeFamilies, DerivingStrategies, GeneralisedNewtypeDeriving #-}

module MappingRep
  ( MapOfRep
  , MoveRep
  , GeneratorRep
  , GmapPerm(..)
  , RepInfo(RepInfo,rep)
  , makeGeneratorRep
  , decodePerm
  , encodePerm
  , getMutation
  , getCrossover
  ) where

---------------------------------------------
import Utils
import Types
---------------------------------------------
import Args (MutMethod(..))
import Mapping (Gmap(..), GmapGenerator(..), Move(..), SepGen, ConstGen)
import Genome (asListOfGenes, getPerm, toEndK)
import Perm (permInerList, renumberToSort)
---------------------------------------------
import Control.Arrow (first, second)
import Control.Monad (replicateM)
import Control.Monad.Random (getRandomR, MonadRandom)
import Data.Array ((!))
import Data.Hashable (Hashable(..))
import Data.Maybe (fromJust)
import Data.Sequence (Seq(..), (><))
import Data.Foldable (toList)
import Data.Functor.Identity (Identity(..))
import Numeric.Probability.Distribution (sample)
import System.Random.Shuffle (shuffleM)
---------------------------------------------
import qualified Data.List as List
import qualified Data.Array as Array
import qualified Data.IntMap as IntMap
import qualified Data.Sequence as Seq
import qualified Numeric.Probability.Distribution as Dis
---------------------------------------------
import qualified Genome as G
---------------------------------------------

newtype PIdx = PIdx Int deriving newtype (Num,Ord,Eq,Real,Enum,Integral,Show,Read)

type MapOfRep = GmapPerm
type MoveRep = Seq PermCode
type GeneratorRep = Identity MapOfRep
type instance (SepGen GeneratorRep MapOfRep) = ConstGen GeneratorRep MapOfRep

data GmapPerm = GmapPerm RepInfo (Seq PermCode)
data RepInfo = RepInfo
  { size :: Size
  , rep :: Rep
  , occ :: [Occ]
  }
type PermCode = Integer

makeGeneratorRep :: RepInfo -> GeneratorRep
makeGeneratorRep ri@RepInfo{..} =
  Identity . GmapPerm ri  $ Seq.replicate (fromIntegral rep) 0

incKPerms :: (MonadRandom mr) => Int -> MapOfRep -> mr MapOfRep
incKPerms k (GmapPerm ri@RepInfo{..} m) = do
    idxs <- shuffleM [0..fromIntegral rep - 1]
    let m' = (\(old,new,_) -> new >< old) . foldl incIdx (m,Seq.Empty,occ) . List.sort . take k $ idxs
    return $ GmapPerm ri m'
      where
        incIdx (l:<|old,new,o:os) idx =
          if Seq.length new == idx
             then let l' = (l + 1) `mod` fac o in (old,new:|>l',os)
             else incIdx (old,new:|>l,os) idx
        incIdx (_,_,_) _ = error patternError

--------------------- Instances ---------------------------------------

instance Show GmapPerm where
  show (GmapPerm RepInfo{..} m) = show . fmap (uncurry decodePerm) . zip occ . toList $ m

instance Eq GmapPerm where
  (==) (GmapPerm _ m1) (GmapPerm _ m2) = m1 == m2

instance Ord GmapPerm where
  (<=) (GmapPerm _ m1) (GmapPerm _ m2) = m1 <= m2
  compare (GmapPerm _ m1) (GmapPerm _ m2) = compare m1 m2

instance Hashable GmapPerm where
  hashWithSalt salt (GmapPerm _ m) = hashWithSalt salt . toList $ m
  hash (GmapPerm _ m) = hash . toList $ m

instance Gmap GmapPerm where
  mapGenome m g = getPerm $ mapGenome' m g

  mapGenome' (GmapPerm RepInfo{..} m) =
    asListOfGenes (snd . List.mapAccumL mapOne IntMap.empty)
    where
      perArr = Array.listArray (0, fromIntegral rep) . zipWith decodePerm occ . toList $ m
      mapOne prevOcc a =
        if ia > fromIntegral rep
        then (prevOcc, a)
        else (prevOcc', toEndK ((perArr ! i) !! ((prevOcc' IntMap.! ia) - 1)) size a)
        where
          prevOcc' = IntMap.alter incOcc ia prevOcc
          incOcc Nothing = Just 1
          incOcc (Just o) = Just $ o + 1
          ia = abs $ fromIntegral a
          i = abs a - 1

  mapSize (GmapPerm RepInfo{..} _) = fromIntegral rep

  recoverMap p g = GmapPerm ri m
    where
      m = Seq.fromList . zipWith encodePerm occ . filter ((1 <) . length) . map (contractNumbers . snd) . separateByKey $ zip lg lp
      lp = map abs $ permInerList p
      lg = map abs $ G.toListOfGenes g
      ri = RepInfo (G.len g) (G.rep g) occ
      occ = G.getOccList . G.occA $ g

  getMutation mm lo hi = case mm of
    MR -> \m -> do
      k <- getRandomR (lo,hi)
      incKPerms k m
    MB -> \(GmapPerm ri@RepInfo{..} m) -> do
      (k,idx) <- second fromIntegral <$> chooseKBlock lo hi rep
      let (l,r) = (idx, idx + k - 1)
      let (beg,end_) = Seq.splitAt (l-1) . Seq.zip (Seq.fromList occ) $ m
      let (mid,end) = Seq.splitAt (r-l+1) end_
      let m' = fmap snd $ beg >< fmap (\(o,x) -> (o,(x + 1) `mod` fac o)) mid >< end
      return $ GmapPerm ri m'

  -- Use same crossover to all (other methods not implemented)
  getCrossover _ lo hi (GmapPerm ri@RepInfo{..} m1) (GmapPerm _ m2) = do
    k_ <- getRandomR (lo,hi)
    let k = (k_ * fromIntegral rep) `div` 100
    bools <- shuffleM (replicate k False ++ replicate (fromIntegral rep - k) True)
    let m' = Seq.zipWith3 choosePerm m1 m2 (Seq.fromList bools)
    return $ GmapPerm ri m'
    where
      choosePerm p1 p2 b = if b then p2 else p1

instance Move (Seq PermCode) GmapPerm where
  newtype MoveRestriction (Seq PermCode) = MoveRestrictionRep [Bool]

  emptyMoveRestriction (GmapPerm RepInfo{..} _) = MoveRestrictionRep $ replicate (fromIntegral rep) False

  getMove (GmapPerm _ m1) (GmapPerm _ m2) = Seq.zipWith (-) m2 m1

  applyMove mov (GmapPerm ri@RepInfo{..} m) = GmapPerm ri $ Seq.zipWith3 f m mov (Seq.fromList occ)
    where f x y o = (x + y) `mod` fac o

  genMovesWithRestriction (MoveRestrictionRep res) (GmapPerm RepInfo{..} _) =
    map (toMov . fst) . filter (not . snd) . zip [0..] $ res
    where toMov idx = (Seq.replicate idx 0 :|> 1)
            >< Seq.replicate (fromIntegral rep - idx - 1) 0

  addRestriction mov (MoveRestrictionRep res) =
    MoveRestrictionRep . zipWith (||) res . map (/= 0) . toList $ mov 

  delRestriction mov (MoveRestrictionRep res) =
    MoveRestrictionRep . zipWith (&&) res . map (== 0) . toList $ mov 

  clearRestriction (MoveRestrictionRep res) = MoveRestrictionRep $ fmap (const False) res


instance GmapGenerator GeneratorRep MapOfRep where
  generateMaps (Identity (GmapPerm ri@RepInfo{..} _)) c = replicateM (fromIntegral c) randMap
    where
      randMap = GmapPerm ri . Seq.fromList <$>
        mapM (\o -> getRandomR (0,fac o - 1)) (take (fromIntegral rep) occ)

  standardMap = runIdentity

  combineMaps _ (GmapPerm ri@RepInfo{..} m1) (GmapPerm _ m2) = GmapPerm ri m'
    where
      p1s = zipWith decodePerm occ (toList m1)
      p2s = zipWith decodePerm occ (toList m2)
      m' = Seq.fromList . zipWith encodePerm occ $ zipWith renumberToSort p1s p2s

  neighbors _ (GmapPerm ri@RepInfo{..} m) = map (GmapPerm ri) $ List.unfoldr go (Empty,m,occ)
    where
      go (_, _, []) = Nothing
      go (_, Empty, _) = Nothing
      go (pref,l:<|suffix,o:os) =
        let l' = (l + 1) `mod` fac o in
            Just (pref >< (l':<|suffix),(pref:|>l,suffix,os))

  randomCombination _ _ [] = error patternError
  randomCombination _ create maps@(GmapPerm ri@RepInfo{..} _:_) =
    map (GmapPerm ri) <$> ms'
    where
      ms = map (\(GmapPerm _ m) -> m) maps

      weights = List.unfoldr go (map (Empty,) ms)
        where
          go [] = Nothing
          go ((_,Empty):_) = Nothing
          go lss = Just . first countReplicas . unzip . shift $ lss
          shift = map (\(pref,l:<|suf) -> (l,(pref:|>l,suf)))
      distributions = map Dis.fromList weights

      ms' = replicateM create newMap
        where newMap = Seq.fromList . map fromJust <$> mapM sample distributions

  flight _ p m@(GmapPerm RepInfo{..} _) = do
    k <- getK 0
    incKPerms (fromIntegral k) m
    where
      getK k = if k == rep then return k else do
        val <- getRandomR (1,100)
        if val <= p then return (k+1) else getK (k+1)


--------------------- Aux Functions -----------------------------------

chooseKBlock :: (MonadRandom mr) => Int -> Int -> Rep -> mr (Int,PIdx)
chooseKBlock lo hi d = do
  k <- getRandomR (lo,hi)
  let (beg,end) = (0, fromIntegral d - 1)
  idx <- getRandomR (beg,end-k+1)
  return (k, fromIntegral idx)

decodePerm :: Occ -> PermCode -> [Int]
decodePerm r = snd . List.mapAccumL pickEl [1..fromIntegral r] . decodeLehmer r
  where
    pickEl l pos = let x = l List.!! pos in (List.delete x l, x)
    decodeLehmer r code =
      if r <= 1
      then [0]
      else pos : decodeLehmer (r - 1) (code `mod` multiplier)
        where
          multiplier = fac (fromIntegral r - 1)
          pos = floor (code `fdiv` multiplier)

encodePerm :: Occ -> [Int] -> PermCode 
encodePerm r = encodeLehmer r . snd . List.mapAccumL getPos [1..fromIntegral r]
  where
    getPos l v = (List.delete v l, fromJust $ List.elemIndex v l)
    encodeLehmer :: Occ -> [Int] -> PermCode
    encodeLehmer r (pos:poss) =
      if r <= 1
      then 0
      else fromIntegral pos * multiplier + encodeLehmer (r - 1) poss
        where
          multiplier = fac (fromIntegral r - 1)
    encodeLehmer _ [] = error patternError

