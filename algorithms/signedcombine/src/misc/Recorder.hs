{-# LANGUAGE FlexibleContexts, GADTs, TemplateHaskell, ConstraintKinds, FlexibleInstances, FunctionalDependencies, TupleSections, TypeFamilies #-}

module Recorder (Recorder, MonadRecorder(..), prepareRecorder, evaluateMap, MonadRandomRecorder, RecMon) where

--------------------------------------------------
import Types
import Utils
--------------------------------------------------
import Mapping (Gmap, PartialGmap(..))
--------------------------------------------------
import Lens.Simple (view, (.~), (&), (%~), makeLenses)
import Control.Monad.Random (MonadRandom)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, get, put, modify)
import Data.List (sort)
import Data.HashMap.Strict (HashMap)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Tuple (swap)
--------------------------------------------------
import qualified Data.HashMap.Strict as Map
--------------------------------------------------

type MonadRandomRecorder m mon = (MonadRecorder m mon, MonadRandom mon)

class Monad mon => MonadRecorder m mon | mon -> m where
  type InerMon mon :: * -> *
  evaluateMaps :: (Gmap m) => [m] -> mon [(Dist,m)]
  getScores :: mon [Dist]
  recordSize :: mon Int
  fullRecord :: mon Bool
  reduceTotal :: Int -> mon ()
  recoverTotal :: mon ()
  resetCount :: mon ()
  remaningToCreate :: mon Int
  updateWithPartialGmap :: (Gmap m, PartialGmap pm, CompletGmap pm ~ m) => pm -> mon [(Dist,m)]

type Scores m = HashMap m (Int,Dist)
data Recorder m mon = Recorder {
   _evaluator :: m -> mon Dist,
   _scoreMap  :: Scores m,
   _count     :: Int,
   _limit     :: Int,
   _total     :: Int,
   _fix_total :: Int}
makeLenses ''Recorder
type RecMon m mon = StateT (Recorder m mon) mon

prepareRecorder :: (m -> mon Dist) -> Int -> Recorder m mon
prepareRecorder eval r = Recorder eval Map.empty 0 500 r r

instance (Monad mon, Ord m) => MonadRecorder m (RecMon m mon) where
  type InerMon (RecMon m mon) = mon
  evaluateMaps ms = do
      rec <- get
      let eval = view evaluator rec
          scores = view scoreMap rec
          r = view total rec
      let getDist scores' m =
            case scores' Map.!? m of
              Nothing -> if Map.size scores' < r
                            then do
                              dist <- eval m
                              return (Map.insert m (Map.size scores',dist) scores', Just (m,dist))
                            else return (scores', Nothing)
              Just (_,dist) -> return (scores', Just (m,dist))
      (scores',maybe_dists) <- lift $ mapAccumM getDist scores ms
      let dists = catMaybes maybe_dists
      put $ rec & scoreMap .~ scores'
                & count %~ if Map.size scores' > Map.size scores then const 0 else id
      return $ map swap dists

  getScores = do
    rec <- get
    let r = view total rec
    return . take r . map snd . sort . Map.elems . view scoreMap $ rec

  recordSize = Map.size . view scoreMap <$> get

  fullRecord = do
    rec <- get
    size <- recordSize
    let r = view total rec
        l = view limit rec
        c = view count rec
        (full, rec')
          | size >= r = (True, rec)
          | c == l = (True, rec)
          | otherwise = (False, rec & count %~ (+1))
    put rec'
    return full

  reduceTotal create = do
    size <- recordSize
    modify (total %~ min (size + create))

  recoverTotal = do
    rec <- get
    put (rec & total .~ view fix_total rec)

  resetCount = do
    rec <- get
    put (rec & count .~ 0)

  remaningToCreate = do
    r <- view total <$> get
    size <- recordSize
    return $ r - size

  updateWithPartialGmap pm = do
    rec <- get
    let sm = view scoreMap rec
        list_sm' = mapMaybe reduceEntry . Map.toList $ sm
        sm' = Map.fromList list_sm'
        rec' = rec & scoreMap .~ sm'
                   & evaluator %~ (. expandGmap pm)
    put rec'
    return . map (\(m,(_,d)) -> (d,m)) $ list_sm'
      where
        reduceEntry (m,v) = (,v) <$> reduceGmap pm m

evaluateMap :: (Gmap m, MonadRecorder m mon) => m -> mon (Dist,m)
evaluateMap m = head <$> evaluateMaps [m]
