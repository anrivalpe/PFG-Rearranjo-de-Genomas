{-# LANGUAGE FlexibleContexts, GADTs, RankNTypes, FlexibleInstances, MultiParamTypeClasses, RecordWildCards #-}

module LocalSearch (LocalData, initializeLocal, exploreNeightbors, makeLocalData, heap) where

--------------------------------------------------
import Utils
import Types
--------------------------------------------------
import Recorder (MonadRecorder(..), MonadRandomRecorder)
import Heuristic (MapHeuristicData(..))
import Args (Local(..))
import Mapping (Gmap, GmapGenerator(neighbors), ScoredMapSet, makeGSS, topMap, removeMap, updateGSS, topScore)
--------------------------------------------------
import Control.Monad.Random (MonadRandom)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import System.Random.Shuffle (shuffleM)

--------------------------------------------------
import qualified Data.Set as Set
--------------------------------------------------

data LocalData gen m = LocalData {
   generator :: gen,
   createInitial :: Int,
   heap :: ScoredMapSet m,
   mapSet :: Set m,
   createEach :: Int,
   dropAfterUse :: Bool
}

instance (Gmap m, GmapGenerator gen m, MonadRandomRecorder m mon) => MapHeuristicData (LocalData gen m) mon m where
  nextGeneration ld@LocalData{..} = fromMaybe ld <$> runMaybeT oneLocalSearch
    where
      oneLocalSearch = do
        (score, best) <- topMap heap
        ms <- lift $ exploreNeightborsUnused ld best
        scored_maps <- lift $ evaluateMaps ms
        let heap' = (`updateGSS` scored_maps) $
                    if dropAfterUse || null ms
                      then removeMap heap (score,best)
                      else heap
        return $ ld {heap = heap', mapSet = mapSet `Set.union` Set.fromList ms}

  totalScore = return . nothingIsMin . fmap negate . topScore . heap

  createOnStart = return . createInitial

  initializeWithMaps scored_maps ld@LocalData{..} = do
    let gss = makeGSS scored_maps
    let mset = Set.fromList . map snd $ scored_maps
    return $ ld {mapSet = mset, heap = gss}

initializeLocal :: (Gmap m, GmapGenerator gen m) => Local -> gen -> LocalData gen m
initializeLocal (Local create_initial create_each) gen =
    let ld = makeLocalData gen [] create_each True in
    ld {createInitial = create_initial}

makeLocalData :: (Gmap m, GmapGenerator gen m) => gen -> [(Dist,m)] -> Int -> Bool -> LocalData gen m
makeLocalData gen scored_maps = LocalData gen 0 heap mset
  where
    mset = Set.fromList . map snd $ scored_maps
    heap = makeGSS scored_maps

exploreNeightborsUnused :: (Gmap m, MonadRandom mon, GmapGenerator gen m) => LocalData gen m -> m -> mon [m]
exploreNeightborsUnused ld = fmap (take create . filter (`Set.notMember` mset)) . exploreNeightbors (generator ld)
  where
    create = createEach ld
    mset = mapSet ld

exploreNeightbors :: (Gmap m, GmapGenerator gen m, MonadRandom mon) => gen -> m -> mon [m]
exploreNeightbors gen = shuffleM . neighbors gen

