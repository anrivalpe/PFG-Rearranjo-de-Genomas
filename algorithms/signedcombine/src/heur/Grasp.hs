{-# LANGUAGE FlexibleContexts, GADTs, RankNTypes, FlexibleInstances, MultiParamTypeClasses, RecordWildCards #-}

module Grasp (GraspData, initializeGrasp) where

--------------------------------------------------
import Utils
--------------------------------------------------
import Recorder (MonadRecorder(..), MonadRandomRecorder)
import Heuristic (MapHeuristicData(..), runHeuristic)
import LocalSearch (makeLocalData, heap)
import SimulatedAnnealing (SimAnnData(scoredMaps), initializeSimAnn)
import Args (Grasp(..), SimAnn(..))
import Mapping (Gmap, GmapGenerator(randomCombination), makeGSS, combineGSS, takeAllMaps, selection, ScoredMapSet, topScore)
--------------------------------------------------

data GraspData gen m = GraspData {
  generator :: gen,
  createInitial :: Int,
  heapG :: ScoredMapSet m,
  createGen :: Int,
  createLocal :: Int,
  rclSize :: Int,
  simAnn :: Maybe (SimAnnData m gen)
}

instance (Gmap m, GmapGenerator gen m, MonadRandomRecorder m mon) => MapHeuristicData (GraspData gen m) mon m where
  nextGeneration gd@GraspData{..} = do
    let rcl = heapG

    -- Generation
    ms <- randomGenerationStage createGen rcl
    scored_maps <- evaluateMaps ms

    -- Local Search
    reduceTotal createLocal
    heap' <- case simAnn of
               Just sd -> do
                   sd' <- initializeWithMaps scored_maps sd
                   sd'' <- runHeuristic sd'
                   return $ makeGSS . scoredMaps $ sd''
               Nothing -> do
                   let ld = makeLocalData generator scored_maps 1 False
                   ld' <- runHeuristic ld
                   return $ heap ld'
    let rcl' = updateRCL heap' rcl
    recoverTotal

    return $ gd {heapG = rcl'}
    where
      randomGenerationStage create = randomCombination generator create . takeAllMaps
      updateRCL heap' = selection rclSize . combineGSS heap'

  totalScore = return . nothingIsMin . fmap negate . topScore . heapG

  createOnStart = return . createInitial

  initializeWithMaps scored_maps gd@GraspData{..} = do
    let rcl = makeRCL scored_maps
    return $ gd {heapG = rcl}
    where
      makeRCL = selection rclSize . makeGSS

initializeGrasp :: (GmapGenerator gen m) => Grasp -> gen -> GraspData gen m
initializeGrasp (Grasp create_initial create_rcl create_local rcl_size simAnn) gen =
    let sd = if simAnn
                then Just $ initializeSimAnn (SimAnn 5 2 1 98) gen
                else Nothing in
    GraspData gen create_initial (makeGSS []) create_rcl create_local rcl_size sd
