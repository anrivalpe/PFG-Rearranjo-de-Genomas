{-# LANGUAGE FlexibleContexts, GADTs, RankNTypes, FlexibleInstances, MultiParamTypeClasses, RecordWildCards #-}

module CuckooSearch (CuckooData, initializeCuckoo) where

--------------------------------------------------
import Utils
--------------------------------------------------
import Heuristic (MapHeuristicData(..))
import Recorder (MonadRecorder(..), MonadRandomRecorder, evaluateMap)
import Args (Cuckoo(..))
import Mapping (Gmap, GmapGenerator(generateMaps, flight), makeGSS, removeMap, selection, updateGSS, takeRandMap, ScoredMapSet, topScore)
--------------------------------------------------

data CuckooData m gen = CuckooData {
  generator :: gen,
  createInitial :: Int,
  heap :: ScoredMapSet m,
  createEach :: Int,
  remaningInGeneration :: Int,
  prob :: Int
}

instance (MonadRandomRecorder m mon, Gmap m, GmapGenerator gen m) => MapHeuristicData (CuckooData m gen) mon m where
  nextGeneration cd@CuckooData{..} =
    if remaningInGeneration <= 0 then do
      let keep = createInitial - 1
      cuckoos <- generateMaps generator 1
      scored_cuckoos <- evaluateMaps cuckoos
      let heap' = (`updateGSS` scored_cuckoos) . selection keep $ heap
      return $ cd {heap = heap', remaningInGeneration = createEach}
    else do
      old@(dist, cuckoo) <- chooseCuckoo heap
      cuckoo' <- changeCuckoo cuckoo
      (dist',_) <- evaluateMap cuckoo'
      let heap' = if dist' < dist
                then updateGSS (removeMap heap old) [(dist', cuckoo')]
                else heap
      return $ cd {heap = heap', remaningInGeneration = remaningInGeneration - 1}
    where
      chooseCuckoo = takeRandMap
      changeCuckoo = flight generator prob

  totalScore = return . nothingIsMin . fmap negate . topScore . heap

  createOnStart = return . createInitial

  initializeWithMaps scored_maps cd = do
    let gss = makeGSS scored_maps
    return $ cd {heap = gss, remaningInGeneration = createEach cd}

initializeCuckoo :: (GmapGenerator gen m) => Cuckoo -> gen -> CuckooData m gen
initializeCuckoo (Cuckoo create_initial create_each p) gen =
  CuckooData gen create_initial (makeGSS []) create_each create_each p
