{-# LANGUAGE FlexibleContexts, GADTs, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, RecordWildCards, RankNTypes #-}

module SeparationMeta (SepMetaData, initializeSepMeta) where

--------------------------------------------------
import Types
--------------------------------------------------
import Meta(useHeurData)
import Recorder (MonadRecorder(..), MonadRandomRecorder, updateWithPartialGmap)
import Heuristic (MapHeuristicData(..), runHeuristic)
import Separation (SepData, generator, initializeSepWithLevel)
import Args (Sep(..), SepMeta(..), MapHeur)
import Mapping (GmapFixer(..), PartialGmap(..), Gmap(mapSize), GmapGenerator(..), WithSep, Move)
--------------------------------------------------
import Control.Exception.Base (throw)
--------------------------------------------------

data SepMetaData gen mon m = forall hd. (MapHeuristicData hd mon m) => SepMetaData {
  createInitial :: Int,
  sepData :: SepData gen m,
  inerHeur :: hd,
  mapHeur :: MapHeur
}

instance (MonadRandomRecorder m mon, CompletGmap sg ~ m, CompletGenerator sg ~ gen, PartialGmap sg, GmapFixer sg m, GmapGenerator sg m, GmapGenerator gen m, Move mov m) => MapHeuristicData (SepMetaData sg mon m) mon m where
  nextGeneration SepMetaData{..} = do
    hd <- nextGeneration inerHeur
    return $ SepMetaData createInitial sepData hd mapHeur

  totalScore SepMetaData{..} = totalScore inerHeur

  createOnStart _ = return 0

  initializeWithMaps _ SepMetaData{..} = do
    -- Run Separation
    reduceTotal createInitial
    sd' <- runHeuristic sepData
    recoverTotal

    -- Update Recorder
    let sepGen = generator sd'
    scored_maps <- updateWithPartialGmap sepGen

    useHeurData mapHeur (\f -> do
      hd <- f (reducedGenerator sepGen)
      hd' <- initializeWithMaps scored_maps hd
      return $ SepMetaData createInitial sd' hd' mapHeur)

initializeSepMeta :: (MonadRandomRecorder m mon, WithSep sg gen m, Move mov m) => SepMeta -> gen -> mon (SepMetaData sg mon m)
initializeSepMeta (SepMeta create_initial level mh) gen = do
    let std = standardMap gen
        s = mapSize std
    sd <- initializeSepWithLevel level Sep gen
    if level > s
    then throw $ InvalidArgument "level > map size" "initializeSepMeta"
    else useHeurData mh (\f -> do
        hd <- f gen
        return $ SepMetaData create_initial sd hd mh)
