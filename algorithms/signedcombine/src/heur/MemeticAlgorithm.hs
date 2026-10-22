{-# LANGUAGE FlexibleContexts, GADTs, RankNTypes, FlexibleInstances, MultiParamTypeClasses, RecordWildCards #-}

module MemeticAlgorithm (MemAlgData, initializeMemAlg) where

--------------------------------------------------
import Utils
--------------------------------------------------
import Recorder (MonadRecorder(..), MonadRandomRecorder)
import Heuristic (MapHeuristicData(..), runHeuristic)
import Args (MemAlg(..), SimAnn(..))
import Mapping (Gmap, GmapGenerator, getCrossover, makeGSS, combineGSS, selection, ScoredMapSet, topScore)
import SimulatedAnnealing (SimAnnData(scoredMaps), initializeSimAnn)
import LocalSearch (makeLocalData, heap)
import GeneticAlgorithm (generateByCrossover)
--------------------------------------------------

data MemAlgData gen m mon = MemAlgData {
  generator :: gen,
  createInitial :: Int,
  heapG :: ScoredMapSet m,
  cross :: m -> m -> mon m,
  keep :: Int,
  generateWithC :: Int,
  createLocal :: Int,
  simAnn :: Maybe (SimAnnData m gen)
 }

instance (MonadRandomRecorder m mon, Gmap m, GmapGenerator gen m) => MapHeuristicData (MemAlgData gen m mon) mon m where
  nextGeneration md@MemAlgData{..} = do
    -- Selection
    let heap' = selection keep heapG

    -- Crossover
    ms_c <- generateByCrossover heap' generateWithC cross
    scored_maps_c <- evaluateMaps ms_c

    -- Local Search
    reduceTotal createLocal
    heap_l <- case simAnn of
               Just sd -> do
                   sd' <- initializeWithMaps scored_maps_c sd
                   sd'' <- runHeuristic sd'
                   return $ makeGSS . scoredMaps $ sd''
               Nothing -> do
                   let ld = makeLocalData generator scored_maps_c 1 False
                   ld' <- runHeuristic ld
                   return $ heap ld'
    let heap'' = combineGSS heap' heap_l
    recoverTotal

    return $ md {heapG = heap''}

  totalScore = return . nothingIsMin . fmap negate . topScore . heapG

  createOnStart = return . createInitial

  initializeWithMaps scored_maps md@MemAlgData{..} = do
    let gss = makeGSS scored_maps
    return $ md {heapG = gss}

initializeMemAlg :: (GmapGenerator gen m, MonadRandomRecorder m mon) => MemAlg -> gen -> mon (MemAlgData gen m mon)
initializeMemAlg (MemAlg create_initial keep loc hic crossover generate_c create_local simAnn) gen =
    let sd = if simAnn
                then Just $ initializeSimAnn (SimAnn 5 2 1 98) gen
                else Nothing in
    let cross = getCrossover crossover loc hic in
    return $ MemAlgData gen create_initial (makeGSS []) cross keep generate_c create_local sd
