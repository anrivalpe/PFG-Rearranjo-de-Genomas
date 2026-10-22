{-# LANGUAGE FlexibleContexts, GADTs, RankNTypes, FlexibleInstances, MultiParamTypeClasses, RecordWildCards #-}

module GeneticAlgorithm (GenAlgData, initializeGenAlg, generateByCrossover, generateByMutation) where

--------------------------------------------------
import Utils
--------------------------------------------------
import Heuristic (MapHeuristicData(..))
import Recorder (MonadRecorder(..), MonadRandomRecorder)
import Args (GenAlg(..))
import Mapping (ScoredMapSet, makeGSS, combineGSS, takeAllMaps, selection, topScore, Gmap, GmapGenerator, getMutation, getCrossover)
--------------------------------------------------
import Control.Monad.Random (MonadRandom)
import System.Random.Shuffle (shuffleM)
--------------------------------------------------

data GenAlgData m mon = GenAlgData {
  createInitial :: Int,
  heap :: ScoredMapSet m,
  mut :: m -> mon m,
  cross :: m -> m -> mon m,
  keep :: Int,
  generateWithM :: Int,
  generateWithC :: Int
}

instance (MonadRandomRecorder m mon, Gmap m) => MapHeuristicData (GenAlgData m mon) mon m where
  nextGeneration gd@GenAlgData{..} = do
    -- Selection
    let heap' = selection keep heap

    -- Crossover
    ms_c <- generateByCrossover heap' generateWithC cross
    scored_maps_c <- evaluateMaps ms_c
    let heap_c = combineGSS heap' (makeGSS scored_maps_c)

    -- Mutation
    ms_m <- generateByMutation heap' generateWithM mut
    scored_maps_m <- evaluateMaps ms_m
    let heap_m = combineGSS heap_c (makeGSS scored_maps_m)

    return $ gd {heap = heap_m}

  totalScore = return . nothingIsMin . fmap negate . topScore . heap

  createOnStart = return . createInitial

  initializeWithMaps scored_maps gd@GenAlgData{..} = do
    let gss = makeGSS scored_maps
    return $ gd {heap = gss}

initializeGenAlg :: (GmapGenerator gen m, MonadRandomRecorder m mon) => GenAlg -> gen -> mon (GenAlgData m mon)
initializeGenAlg (GenAlg create_initial keep lom him loc hic mutation crossover generate_m generate_c) _ =
  let mut = getMutation mutation lom him
      cross = getCrossover crossover loc hic in
  return $ GenAlgData create_initial (makeGSS []) mut cross keep generate_m generate_c

generateByCrossover :: (MonadRandom mon) => ScoredMapSet m -> Int -> (m -> m -> mon m) -> mon [m]
generateByCrossover gss generate cross = do
  ms <- shuffleM . takeAllMaps $ gss
  mapM (uncurry cross) . take generate . uncurry zip . foldr (\a ~(x,y) -> (a:y,x)) ([],[]) $ ms

generateByMutation :: (MonadRandom mon) => ScoredMapSet m -> Int -> (m -> mon m) -> mon [m]
generateByMutation gss generate mut = do
  ms <- shuffleM . takeAllMaps $ gss
  mapM mut . take generate $ ms

