{-# LANGUAGE FlexibleContexts, GADTs, RankNTypes, FlexibleInstances, MultiParamTypeClasses, RecordWildCards #-}

module SimulatedAnnealing (SimAnnData, scoredMaps, initializeSimAnn, setMapSimAnn) where

--------------------------------------------------
import Utils
import Types
--------------------------------------------------
import Recorder (MonadRandomRecorder, evaluateMap)
import Heuristic (MapHeuristicData(..))
import LocalSearch (exploreNeightbors)
import Args (SimAnn(..))
import Mapping (Gmap, GmapGenerator(..))
--------------------------------------------------
import Control.Monad.Random (getRandomR)
--------------------------------------------------

data SimAnnData m gen = SimAnnData {
  generator :: gen,
  noImprovIter :: Int,
  maxIter :: Int,
  constK :: Double,
  constA :: Int,
  currentTemp :: Double,
  initialTemp :: Double,
  best :: Dist,
  currentScoredMap :: (Dist,m),
  currentNeightbors :: [m],
  scoredMaps :: [(Dist,m)]
}

instance (Gmap m, MonadRandomRecorder m mon, GmapGenerator gen m) =>
         MapHeuristicData (SimAnnData m gen) mon m where

    nextGeneration sd = do
        maybe_sd <- maybeNextGeneration sd
        case maybe_sd of
            Nothing -> error patternError
            Just sd' -> return sd'

    maybeNextGeneration sd@SimAnnData {..} =
        if constK * currentTemp < 0.1
            then maybeNextGeneration $ sd {currentTemp = initialTemp}
            else do
                let (dist, m) = currentScoredMap
                    temp = currentTemp
                vs_ <- (\l -> if null l
                                 then exploreNeightbors generator m
                                 else return l) currentNeightbors
                if null vs_ then return Nothing else do
                    let (v:vs) = vs_
                    (dist', m') <- evaluateMap v
                    let scoredMaps' = (dist', m') : scoredMaps
                    let delta_dist = dist' - dist
                    x <- getRandomR (0, 1)
                    let prob = exp (-delta_dist `fdiv` (constK * temp)) / 2
                    if delta_dist < 0 || null vs || x <= prob
                        then let (temp', no_improv_iter', best')
                                     | dist' < best = (temp, 0, dist')
                                     | noImprovIter + 1 >= maxIter =
                                         (constA `fdiv` (100.0 :: Double) * temp, 0, best)
                                     | otherwise = (temp, noImprovIter + 1, best)
                              in return . Just $
                                 sd
                                     { noImprovIter = no_improv_iter'
                                     , currentTemp = temp'
                                     , best = best'
                                     , currentScoredMap = (dist', m')
                                     , currentNeightbors = []
                                     , scoredMaps = scoredMaps'
                                     }
                        else return . Just $
                             sd
                                 { noImprovIter = noImprovIter + 1
                                 , currentNeightbors = vs
                                 , scoredMaps = scoredMaps'
                                 }

    totalScore = return . negate . best

    createOnStart _ = return 1

    initializeWithMaps scored_maps sd@SimAnnData {..} = do
        let sm = minWith fst scored_maps
        return . setMapSimAnn sm $ sd {noImprovIter = 0, currentTemp = initialTemp}

setMapSimAnn :: (sd ~ SimAnnData m gen) => (Dist,m) -> sd -> sd
setMapSimAnn (dist,m) sd@SimAnnData{..} =
  sd {best = min best dist, currentScoredMap = (dist,m), currentNeightbors = [], scoredMaps = []}

initializeSimAnn :: (GmapGenerator gen m) => SimAnn -> gen -> SimAnnData m gen
initializeSimAnn (SimAnn max_iter temp0 k a) gen = do
  let std = standardMap gen
  SimAnnData gen 0 max_iter k a (fromIntegral temp0) (fromIntegral temp0) maxBound (maxBound,std) [] []
