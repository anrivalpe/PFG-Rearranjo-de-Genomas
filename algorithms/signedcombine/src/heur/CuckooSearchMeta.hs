{-# LANGUAGE FlexibleContexts, GADTs, RankNTypes, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, RecordWildCards  #-}

module CuckooSearchMeta (CuckooMetaData, initializeCuckooMeta) where

--------------------------------------------------
import Meta(useHeurData)
import Heuristic (MapHeuristicData(..))
import Recorder (MonadRandomRecorder)
import Mapping (GmapGenerator, Move)
import Args(CuckooMeta(..))
--------------------------------------------------
import Control.Arrow ((***))
import Control.Monad (replicateM)
import System.Random.Shuffle (shuffleM)
--------------------------------------------------
import qualified Data.List as List
--------------------------------------------------

data CuckooMetaData mon m gen = forall hd. (MapHeuristicData hd mon m) => CuckooMetaData {
  generator :: gen,
  createEach :: Int,
  remaningUntilChange :: Int,
  nests :: [hd]
}

instance (MonadRandomRecorder m mon, GmapGenerator gen m) => MapHeuristicData (CuckooMetaData mon m gen) mon m where
  nextGeneration CuckooMetaData{..} =
    if remaningUntilChange <= 0 then do
      scores <- mapM totalScore nests
      let worse_score = minimum scores
      let (worse_nests,better_nests) = (map snd *** map snd) . List.partition ((== worse_score) . fst) . zip scores $ nests
      shuffle_nests <- shuffleM worse_nests
      cuckoo <- randomRestart generator . head $ shuffle_nests
      let nests' = cuckoo:(tail shuffle_nests ++ better_nests)

      return $ CuckooMetaData generator createEach createEach nests'
    else do
      nests' <- changeNest =<< shuffleM nests
      return $ CuckooMetaData generator createEach  (remaningUntilChange - 1) nests'
        where
          changeNest (x:xs) = do
            x' <- nextGeneration x
            return (x':xs)
          changeNest [] = return []

  totalScore CuckooMetaData {nests = nts} = fmap maximum . mapM totalScore $ nts

  createOnStart _ = return 0

  initializeWithMaps _ CuckooMetaData{..} = do
    nests' <- mapM (randomRestart generator) nests
    return $ CuckooMetaData generator createEach createEach nests'

initializeCuckooMeta :: (MonadRandomRecorder m mon, GmapGenerator gen m, Move mov m) => CuckooMeta -> gen -> mon (CuckooMetaData mon m gen)
initializeCuckooMeta (CuckooMeta create_initial create_each mh) gen =
  useHeurData mh (\f -> do
    hds <- replicateM create_initial (f gen)
    return $ CuckooMetaData gen create_each create_each hds)

