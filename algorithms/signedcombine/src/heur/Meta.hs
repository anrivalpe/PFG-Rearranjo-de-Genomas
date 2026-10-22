{-# LANGUAGE FlexibleContexts, GADTs, RankNTypes, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, TypeApplications #-}

module Meta (useHeurData) where

--------------------------------------------------
import Types
--------------------------------------------------
import RandomMaps (initializeMap)
import LocalSearch (initializeLocal)
import TabuSearch (initializeTabu)
import CuckooSearch (initializeCuckoo)
import Grasp (initializeGrasp)
import GeneticAlgorithm (initializeGenAlg)
import MemeticAlgorithm (initializeMemAlg)
import SimulatedAnnealing (initializeSimAnn)
import Heuristic (MapHeuristicData(..))
import Recorder (MonadRandomRecorder)
import Mapping (GmapGenerator, Move)
import Args(MapHeur(..))
--------------------------------------------------
import Control.Exception.Base (throw)
--------------------------------------------------

useHeurData :: (GmapGenerator gen m, MonadRandomRecorder m mon, Move mov m) => MapHeur -> (forall hd. (MapHeuristicData hd mon m) => (gen -> mon hd) -> mon a) -> mon a
useHeurData mh f =
  case mh of
    (A2 com) -> f $ return . initializeMap com
    (A4 com) -> f $ return . initializeLocal com
    (A7 com) -> f $ return . initializeTabu com
    (A14 com) -> f $ return . initializeCuckoo com
    (A5 com) -> f $ return . initializeGrasp com
    (A3 com) -> f $ initializeGenAlg com
    (A15 com) -> f $ initializeMemAlg com
    (A8 com) -> f $ return . initializeSimAnn com
    (A9 _) -> throw $ OtherError "Cannot use Sep inside a meta variation use normal Sep" "useHeurData"
    (BX _) -> throw $ OtherError "Cannot call a meta variation from mata variation use normal cuckoo" "useHeurData"
    (CX _) -> throw $ OtherError "Cannot call a meta variation from mata variation use normal separation" "useHeurData"
