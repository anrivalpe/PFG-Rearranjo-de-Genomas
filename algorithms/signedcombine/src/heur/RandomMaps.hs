{-# LANGUAGE FlexibleContexts, GADTs, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module RandomMaps (MapData, initializeMap) where

--------------------------------------------------
import Utils
import Types
--------------------------------------------------
import Recorder (MonadRecorder(..), MonadRandomRecorder)
import Heuristic (MapHeuristicData(..), generateAndEvaluateMaps)
import Args (RM(..))
import Mapping (GmapGenerator(..))
--------------------------------------------------

data MapData gen m = MapData gen (Maybe Dist)

instance (MonadRandomRecorder m mon, GmapGenerator gen m) => MapHeuristicData (MapData gen m) mon m where
  nextGeneration (MapData gen best) = do
    remaning <- remaningToCreate
    let create = min remaning 20
    scores <- map fst <$> generateAndEvaluateMaps gen create
    let best' = case best of
                  Just score -> minimum (score:scores)
                  Nothing -> minimum scores
    return $ MapData gen (Just best')

  totalScore (MapData _ best) = return . nothingIsMin . fmap negate $ best

  createOnStart _ = return 0

  initializeWithMaps _ = return

initializeMap :: (GmapGenerator gen m) => RM -> gen -> MapData gen m
initializeMap RM gen = MapData gen Nothing
