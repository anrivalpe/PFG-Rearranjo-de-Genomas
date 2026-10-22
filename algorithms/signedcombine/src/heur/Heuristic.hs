{-# LANGUAGE FunctionalDependencies #-}

module Heuristic (MapHeuristicData(..), runHeuristic, generateAndEvaluateMaps) where

------------------------------------
import Types
------------------------------------
import Recorder (MonadRecorder(..), MonadRandomRecorder)
import Mapping (GmapGenerator(..))
------------------------------------

class (MonadRandomRecorder m mon) => MapHeuristicData heur mon m | heur -> m where
  nextGeneration :: heur -> mon heur -- run next generation of the heuristic
  maybeNextGeneration :: heur -> mon (Maybe heur) -- nextGeneration with possibility to stop early
  totalScore :: heur -> mon Dist -- the score that represents the quality of the current status
  createOnStart :: heur -> mon Int -- how many maps to create on the begging
  initializeWithMaps :: [(Dist,m)] -> heur -> mon heur -- use already evaluated maps to initialize heuristic
  randomRestart :: (GmapGenerator gen m) => gen -> heur -> mon heur -- random initialization of heuristic

  maybeNextGeneration = fmap Just . nextGeneration

  randomRestart gen hd = do
    create <- createOnStart hd
    scored_maps <- generateAndEvaluateMaps gen create
    initializeWithMaps scored_maps hd

runHeuristic :: (MapHeuristicData hd mon m, MonadRecorder m mon) => hd -> mon hd
runHeuristic hd = do
  full <- fullRecord
  if full
     then return hd
     else do
         maybe_hd' <- maybeNextGeneration hd
         case maybe_hd' of
           Nothing -> return hd
           Just hd' -> runHeuristic hd'

generateAndEvaluateMaps :: (MonadRandomRecorder m mon, GmapGenerator gn m) => gn -> Int -> mon [(Dist, m)]
generateAndEvaluateMaps gen create = do
  ms <- generateMaps gen create
  evaluateMaps ms
