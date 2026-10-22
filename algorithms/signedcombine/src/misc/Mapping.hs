{-# LANGUAGE FlexibleInstances, TypeFamilies, FunctionalDependencies, FlexibleContexts, DerivingStrategies, GeneralisedNewtypeDeriving, ConstraintKinds #-}

module Mapping
  ( Perm
  , Gmap(..)
  , PartialGmap(..)
  , Move(..)
  , GmapGenerator(..)
  , GmapFixer(..)
  , RestrictedGmapGenerator
  , SepGen
  , WithSep
  , ConstGen(..)
  , ScoredMapSet
  , makeGSS
  , emptyGSS
  , combineGSS
  , topMap
  , topScore
  , removeMap
  , takeTopMaps
  , takeRandMap
  , takeAllMaps
  , selection
  , updateGSS
  ) where

---------------------------------------------
import Types
---------------------------------------------
import Args (MutMethod, CrossMethod)
import Perm (Perm)
import Genome(Genome)
---------------------------------------------
import Control.Arrow (second)
import Data.Hashable (Hashable)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Random (MonadRandom, getRandomR)
import Data.Heap (MinPrioHeap)
import Data.Maybe (fromJust)
import Data.Map (Map)
---------------------------------------------
import qualified Data.Heap as Heap
---------------------------------------------

class (Eq m, Ord m, Show m, Hashable m) => Gmap m where
  mapGenome :: (Genome g a) => m -> g a -> Perm -- map a genome into a permutation
  mapGenome' :: (Genome g a) => m -> g a -> g a -- like mapGenome', but returns a genome
  mapSize :: m -> Int -- size of the map
  recoverMap :: (Genome g a) => Perm -> g a -> m -- inverse of mapGenome
  getMutation :: (MonadRandom mr) => MutMethod -> Int -> Int -> (m -> mr m)
  getCrossover :: (MonadRandom mr) => CrossMethod -> Int -> Int -> (m -> m -> mr m)

class (Show pm) => PartialGmap pm where
  type CompletGmap pm :: *
  type CompletGenerator pm :: *
  mapParGenome :: (Genome g a) => pm -> g a -> g a -- map some genes of a genome
  reduceGmap :: pm -> CompletGmap pm -> Maybe (CompletGmap pm) -- reduce the number of genes mapped by a Gmap, returns Nothing if the Gmap is incompatible with the partial map
  expandGmap :: pm -> CompletGmap pm -> CompletGmap pm -- add partially mapped genes to the Gmap
  reducedGenerator :: pm -> CompletGenerator pm -- generator for reduced maps

class Move mov m | mov -> m, m -> mov where
  data MoveRestriction mov -- a data type representing a restriction on the possible moves
  emptyMoveRestriction :: m -> MoveRestriction mov
  getMove :: m -> m -> mov -- a mov that transforms the first map into the second
  applyMove :: mov -> m -> m -- apply a move into the given map
  genMovesWithRestriction :: MoveRestriction mov -> m -> [mov] -- generate some moves respecting the restriction
  addRestriction :: mov -> MoveRestriction mov -> MoveRestriction mov -- the restriction exclude at least the given move
  delRestriction :: mov -> MoveRestriction mov -> MoveRestriction mov -- the restriction allows at least the given move
  clearRestriction :: MoveRestriction mov -> MoveRestriction mov -- remove all move restrictions

class (Gmap m) => GmapGenerator gn m | gn -> m where
  generateMaps :: (MonadRandom mr) => gn -> Int -> mr [m] -- generate a random list with the requested number of maps
  standardMap :: gn -> m -- the standard map of the given generator
  neighbors :: gn -> m -> [m] -- the neighbors of a map
  combineMaps :: gn -> m -> m -> m -- modify maps so that the second can be raplace by the standard map
  randomCombination :: (MonadRandom mon) => gn -> Int -> [m] -> mon [m] -- a way of randomly combine a list of maps to generate new maps
  flight :: (MonadRandom mon) => gn -> Int -> m -> mon m -- use a constant to generate new map closer to the original map

class GmapFixer fix m | fix -> m where
  reset :: fix -> fix -- reset structure to initial state
  fix :: fix -> Map m Dist -> (fix, Map m Dist) -- fix some structure based on a list of scored maps, useful together with a generator, return only maps that are compatible with fixed structure

type RestrictedGmapGenerator fg m = (GmapFixer fg m, GmapGenerator fg m, Wrapped fg, GmapGenerator (Unwrapped fg) m)

type WithSep sg gen m = (SepGen gen m ~ sg, RestrictedGmapGenerator sg m, Unwrapped sg ~ gen, PartialGmap sg, CompletGenerator sg ~ gen, CompletGmap sg ~ m)
type family SepGen gen m

-------------------------- Instances ----------------------------------

data ConstGen gen m = ConstGen gen m
instance (Show m) => Show (ConstGen gen m) where
    show (ConstGen _ m) = "ConstGen" ++ show m

instance (Show m) => PartialGmap (ConstGen gen m) where
    type CompletGmap (ConstGen gen m) = m
    type CompletGenerator (ConstGen gen m) = gen
    mapParGenome _ = id
    reduceGmap _ = Just
    expandGmap _ = id
    reducedGenerator (ConstGen gen _) = gen

instance (Gmap m, GmapGenerator gen m) => GmapGenerator (ConstGen gen m) m where
    generateMaps (ConstGen _ m) r = return $ replicate r m
    standardMap (ConstGen gen _) = standardMap gen
    neighbors (ConstGen gen _) = neighbors gen
    combineMaps (ConstGen gen _) = combineMaps gen
    randomCombination (ConstGen gen _) = randomCombination gen
    flight (ConstGen gen _) = flight gen

instance GmapFixer (ConstGen gen m) m where
    reset = id
    fix f ms = (f,ms)

instance (GmapGenerator gen m) => Wrapped (ConstGen gen m) where
    type Unwrapped (ConstGen gen m) = gen
    wrap gen = ConstGen gen (standardMap gen)
    unwrap (ConstGen gen _) = gen

-------------------------- ScoredMapSet ----------------------------------

newtype ScoredMapSet m = SM (MinPrioHeap Dist m) deriving (Show)

makeGSS :: [(Dist,m)] -> ScoredMapSet m -- convert a list of scored maps to a set of scored maps
makeGSS = SM . Heap.fromList

emptyGSS :: ScoredMapSet m -> Bool -- a empty set
emptyGSS (SM heap) = null heap

combineGSS :: ScoredMapSet m -> ScoredMapSet m -> ScoredMapSet m -- combine two sets of scored maps
combineGSS (SM heap1) (SM heap2) = SM $ Heap.union heap1 heap2

topMap :: (MonadRandom mon) => ScoredMapSet m -> MaybeT mon (Dist,m) -- the map with the best score
topMap gss@(SM heap) = do
  top <- MaybeT . return $ topScore gss
  let bests = Heap.takeWhile (\(score,_) -> score == top) heap
  idx <- lift $ getRandomR (0, length bests - 1)
  return $ bests !! idx

topScore :: ScoredMapSet m -> Maybe Dist -- best know score
topScore (SM heap) = fst <$> Heap.viewHead heap

removeMap :: (Eq m) => ScoredMapSet m -> (Dist,m) -> ScoredMapSet m -- remove a scored map from the set
removeMap (SM heap) (score_a, a) = SM $ Heap.union tail_sm front_sm
  where
    check (score,m) = score <= score_a && m /= a
    (front, tail_sm) = second (fromJust . Heap.viewTail) $ Heap.span check heap
    front_sm = Heap.fromList front

takeTopMaps :: ScoredMapSet m -> Int -> [m] -- take o given number of the maps with best scores
takeTopMaps (SM heap) k = snd <$> Heap.take k heap

takeRandMap :: (MonadRandom mon) => ScoredMapSet m -> mon (Dist,m)
takeRandMap (SM heap) = do
  idx <- getRandomR (1, Heap.size heap)
  return . last . Heap.take idx $ heap

takeAllMaps :: ScoredMapSet m -> [m] -- take a list with all the maps
takeAllMaps (SM heap) = snd <$> Heap.toList heap

selection :: Int -> ScoredMapSet m -> ScoredMapSet m -- remove all but the given number of maps with best scores
selection n (SM heap) = SM . Heap.fromList . Heap.take n $ heap

updateGSS :: ScoredMapSet m -> [(Dist,m)] -> ScoredMapSet m
updateGSS gss = combineGSS gss . makeGSS
