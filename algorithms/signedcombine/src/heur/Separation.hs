{-# LANGUAGE FlexibleContexts, GADTs, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, RecordWildCards #-}

module Separation (SepData, initializeSep, generator, initializeSepWithLevel) where

--------------------------------------------------
import Types
import Utils
--------------------------------------------------
import Recorder (MonadRecorder(..), MonadRandomRecorder, MonadRandomRecorder)
import Heuristic (MapHeuristicData(..))
import Args (Sep(..))
import Mapping (Gmap(mapSize), GmapFixer(..), GmapGenerator(..), RestrictedGmapGenerator, WithSep)
--------------------------------------------------
import Control.Exception.Base (throw)
import Data.Map (Map)
import Data.Tuple (swap)
--------------------------------------------------
import qualified Data.List as List
import qualified Data.Map as Map
--------------------------------------------------

data SepData gen m = SepData {
  generator :: gen,
  level :: Int,
  size :: Int,
  best :: Maybe Dist,
  createInIters :: [Int],
  scoredMaps :: Map m Dist
}

instance (MonadRandomRecorder m mon, GmapFixer gen m, GmapGenerator gen m) => MapHeuristicData (SepData gen m) mon m where
  nextGeneration sd@SepData{..} =
    case createInIters of
      [] -> nextGeneration =<< randomRestart generator sd
      (create:creates) -> do
        let gen = generator
        ms <- generateMaps gen create
        new_scored_maps <- evaluateMaps ms
        let scores = map fst new_scored_maps
            scored_maps = Map.fromList (swap <$> new_scored_maps) `Map.union` scoredMaps
            (gen',compatible_smaps) = fix gen scored_maps
            best' = case best of
                      Just score -> minimum (score:scores)
                      Nothing -> minimum scores
        return $ sd {generator = gen', best = Just best', createInIters = creates, scoredMaps = compatible_smaps}

  totalScore = return . nothingIsMin . fmap negate . best

  createOnStart _ = return 0

  initializeWithMaps _ sd@SepData{..} = do
    remaning <- remaningToCreate
    let creates = reverse . snd . List.mapAccumL getCreate remaning $ [size - level + 1..size]
    return $ sd {generator = reset generator, createInIters = creates}
      where
        getCreate remaining pos = (remaining - create, create)
          where create
                  | pos == size = remaining
                  | 2 ** fromIntegral pos < fromIntegral create_uniform = 2^pos
                  | otherwise = create_uniform
                create_uniform = remaining `div` (size - pos + 1)

initializeSep :: (MonadRandomRecorder m mon, WithSep sg gen m) => Sep -> gen -> mon (SepData sg m)
initializeSep sep gen = do
    let std = standardMap gen
        s = mapSize std
    initializeSepWithLevel s sep gen

initializeSepWithLevel :: (MonadRandomRecorder m mon, RestrictedGmapGenerator rgen m, gen ~ Unwrapped rgen) => Int -> Sep -> gen -> mon (SepData rgen m)
initializeSepWithLevel level Sep gen = do
  remaning <- remaningToCreate
  let std = standardMap gen
      s = mapSize std
  if remaning < fromIntegral level
  then throw $ InvalidArgument "create_total < level" "separation"
  else return $ SepData (wrap gen) level s Nothing [] Map.empty
