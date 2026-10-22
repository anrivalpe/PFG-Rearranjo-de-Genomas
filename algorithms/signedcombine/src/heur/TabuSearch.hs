{-# LANGUAGE FlexibleContexts, GADTs, RankNTypes, FlexibleInstances, MultiParamTypeClasses, RecordWildCards #-}

module TabuSearch (TabuData, initializeTabu, dropTabu, emptyTabu, addTabu, isEmptyTabu) where

--------------------------------------------------
import Utils
import Types
--------------------------------------------------
import Recorder (MonadRecorder(..), MonadRandomRecorder)
import Heuristic (MapHeuristicData(..))
import Args (Tabu(..))
import Mapping (Gmap(mapSize), GmapGenerator(..), Move(..), MoveRestriction)
--------------------------------------------------
import Data.Sequence (Seq(..))
import System.Random.Shuffle (shuffleM)
--------------------------------------------------
import qualified Data.Sequence as Seq
--------------------------------------------------

data TabuData m mov = TabuData {
  createEach :: Int,
  tabuSize :: Int,
  currentMap :: m,
  best :: Dist,
  tabu :: TabuList mov
}

instance (Gmap m, Move mov m, MonadRandomRecorder m mon) => MapHeuristicData (TabuData m mov) mon m where
  nextGeneration td@TabuData{..} = do
    let m = currentMap
    ms <- exploreNeightborsTabu tabu createEach m
    scored_maps <- evaluateMaps ms
    let best' = minimum . map fst $ scored_maps
        m' = snd . head . filter ((== best') . fst) $ scored_maps
    let tl' = updateTabu tabuSize m m' tabu
    return $ td {currentMap = m', best = min best best', tabu = tl'}
      where
        exploreNeightborsTabu tabu create = fmap (take create) . shuffleM . neighborsTabu tabu

  totalScore = return . negate . best

  createOnStart _ = return 1

  initializeWithMaps scored_maps td@TabuData{..} = do
    let (dist,m) = minWith fst scored_maps
    return $ td {currentMap = m, best = dist, tabu = clearTabu tabu}

initializeTabu :: (Gmap m, Move mov m, GmapGenerator gen m) => Tabu -> gen -> TabuData m mov
initializeTabu (Tabu create_each tabu_size_) gen =
  let std = standardMap gen
      s = mapSize std
      tabu_size = if tabu_size_ >= s then s - 1 else tabu_size_ in
  TabuData create_each tabu_size std maxBound (emptyTabu std)

data TabuList mov = TabuList (Seq mov) (MoveRestriction mov)

isEmptyTabu :: (Move mov m) => TabuList mov -> Bool
isEmptyTabu (TabuList tabu_list _) = null tabu_list

clearTabu :: (Move mov m) => TabuList mov -> TabuList mov
clearTabu (TabuList _ res) = TabuList Seq.Empty (clearRestriction res)

emptyTabu :: (Move mov m) => m -> TabuList mov
emptyTabu = TabuList Seq.Empty . emptyMoveRestriction

updateTabu :: (Move mov m) => Int -> m -> m -> TabuList mov -> TabuList mov
updateTabu tabu_size old_map new_map tabu = tabu'
  where
    tabu'_ = if sizeTabu tabu >= tabu_size
                then dropTabu tabu
                else tabu
    tabu' = if tabu_size == 0
               then tabu'_
               else addTabu old_map new_map tabu'_

addTabu :: (Move mov m) => m -> m -> TabuList mov -> TabuList mov
addTabu old new (TabuList tabu_list res) = TabuList (mov :<| tabu_list) (addRestriction mov res)
  where mov = getMove old new

dropTabu :: (Move mov m) => TabuList mov -> TabuList mov
dropTabu (TabuList (tabu_list :|> old) res) = TabuList tabu_list (delRestriction old res)
dropTabu tabu@(TabuList Seq.Empty _) = tabu

sizeTabu :: TabuList mov -> Int
sizeTabu (TabuList tabu_list _) = Seq.length tabu_list

neighborsTabu :: (Move mov m) => TabuList mov -> m -> [m]
neighborsTabu (TabuList _ res) m = applyMove <$> genMovesWithRestriction res m <*> [m]
