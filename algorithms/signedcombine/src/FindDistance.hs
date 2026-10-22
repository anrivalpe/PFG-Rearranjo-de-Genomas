{-# LANGUAGE ScopedTypeVariables, TupleSections, TypeApplications, FlexibleContexts, GADTs, Rank2Types, MultiWayIf #-}

module FindDistance (findDistance) where

------------------------------------------------------
import Types
------------------------------------------------------
import SimplifyGenomes (simplifyGenomes, partitionIsSigned)
import MCSP (ContractableGenerator(ContGen))
import PermDist (permDist, makeDistEstimator)
import RandomMaps (initializeMap)
import LocalSearch (initializeLocal)
import TabuSearch (initializeTabu)
import CuckooSearch (initializeCuckoo)
import CuckooSearchMeta (initializeCuckooMeta)
import Grasp (initializeGrasp)
import GeneticAlgorithm (initializeGenAlg)
import MemeticAlgorithm (initializeMemAlg)
import SimulatedAnnealing (initializeSimAnn)
import Separation (initializeSep)
import SeparationMeta (initializeSepMeta)
import Extremities (extremities)
import GeneralizedBP (generalizedBP)
import Algs (soar, hittingSet, cycleMap, cycleDec, soarMap)
import Recorder (prepareRecorder, getScores, MonadRandomRecorder, Recorder, evaluateMap, RecMon)
import Heuristic (MapHeuristicData, runHeuristic, initializeWithMaps, createOnStart, generateAndEvaluateMaps)
import Args(Commands(..), MapHeur(..), SingleDist(..), DistArgs, PermDist(..), InitialMap(..), Soar(..))
import Genome (Gene, SGene, UGene, GenomeType(..))
import Perm (PermData, MonadRandomDist)
import Instance (getRepInfo)
import Mapping (Gmap(..), GmapGenerator(..), Move, WithSep)
import MappingDup (makeGeneratorDup, MapOfDup)
import MappingRep (makeGeneratorRep, MapOfRep)
------------------------------------------------------
import Control.Exception.Base (throw)
import Control.Monad (void)
import Control.Monad.Trans.State.Strict (evalStateT)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Proxy (Proxy(..))
------------------------------------------------------
import qualified Instance as I
import qualified ArgsMCSP as MCSP
------------------------------------------------------

-- Given a pair of strings, find a list of upper bounds to their distances:
-- Arguments:
--      force_limit: weather we must generate exactly createTotal maps
--      bs_genomes: the genomes
--      pd: information to calculate the distance on permutations
--      isSign: whether the genomes are signed
--      model: selected rearrange model
--      any_com: information to select and configure the heuristic/algorithm
--      createTotal: total number of mappings to create (if using heuristic based on mappings)
--      onlyDup: whether the strings do not have more than 2 copies of each gene
--      withMap: whether a specific map will be used on the start (and which map)
--      contNei: whether to use the neighbohood based on contracted representation
--      useAss: whether to apply the initial assigments
findDistance :: forall mon. (MonadRandomDist mon) => Bool -> (ByteString, ByteString) -> PermData -> DistArgs -> mon [Dist]
findDistance force_limit bs_genomes_ pd (isSign', model, any_com, createTotal, onlyDup, withMap, contNei, useAss, mcspCom) =
  case isSign of -- select correct type for genes
    Signed -> findDistance_ (Proxy @SGene)
    Unsigned -> findDistance_ (Proxy @UGene)
  where

    -- Update genomes if using MCSP before run the heuristic/algorithm
    mcspArgs = let variation = case (isSign',model) of
                                 (Signed,Rev) -> MCSP.Signed
                                 (Unsigned,Rev) -> MCSP.Reverse
                                 (Unsigned,Trans) -> MCSP.Direct
                                 (Signed,Trans) -> throw (GenomesWithSign "findDistance")
                                 (Signed,TransRev) -> MCSP.Signed
                                 (Unsigned,TransRev) -> MCSP.Reverse
                                 in (variation, mcspCom, onlyDup, useAss)
    isSign = partitionIsSigned mcspArgs
    bs_genomes = simplifyGenomes bs_genomes_ mcspArgs

    findDistance_ :: forall a. (Gene a) => Proxy a -> mon [Dist]
    findDistance_ _ = case any_com of
      MH hm -> if onlyDup 
                  then findDistanceMapHeur hm inst_d
                  else findDistanceMapHeur hm inst_rl
      SD (A0 com) -> callSingleShot (permDist com) inst_pl
      SD (A1 com) -> callSingleShot (extremities com) inst_rs
      SD (A12 com) -> callSingleShot (generalizedBP com) inst_rs
      SD (A6 com) -> callSingleShot (soar com) inst_rl
      SD (A16 com) -> callSingleShot (cycleDec com) inst_rl
      SD (A11 com) -> callSingleShot (hittingSet com) inst_rl
      where
        inst_pl = I.readCorrectInstance I.IsPerm GList isSign model bs_genomes 
        inst_rs = I.readCorrectInstance I.IsStr GSeq isSign model bs_genomes 
        inst_rl = I.readCorrectInstance I.IsStr GList isSign model bs_genomes 
        inst_d = I.readCorrectInstance I.IsDup GList isSign model bs_genomes 
        callSingleShot heur = fmap (:[]) . heur . (,pd) -- algorithms returning a single value

    -- For mapping based heuristics.
    findDistanceMapHeur :: MapHeur -> I.AnyInstance -> mon [Dist]
    findDistanceMapHeur mh (anyInst@(I.AnyInstance inst)) =
      case I.checkPerm inst of
        Just perm_inst -> (:[]) <$> permDist PermDist (I.AnyInstance perm_inst,pd)
        Nothing
          | onlyDup && contNei ->
              useHeurData mh (ContGen inst genDup) (recorder inst) heurBodyDup
          | onlyDup && not contNei -> useHeurData mh genDup (recorder inst) heurBodyDup
          | not onlyDup && contNei ->
              useHeurData mh (ContGen inst genRep) (recorder inst) heurBodyRep
          | otherwise -> useHeurData mh genRep (recorder inst) heurBodyRep
      where
        m_cycle = cycleMap anyInst
        m_soar = soarMap (Soar Ap4 False False) anyInst
        ri = getRepInfo inst
        d = I.dup inst
        genDup = makeGeneratorDup d
        genRep = makeGeneratorRep ri

        heurBodyDup :: (MonadRandomRecorder MapOfDup mon', GmapGenerator gen MapOfDup, MapHeuristicData hd mon' MapOfDup) => gen -> hd -> mon' [Dist]
        heurBodyDup gen hd = do
            create <- createOnStart hd
            scored_maps' <-
                case withMap of
                  WithRand -> generateAndEvaluateMaps gen create
                  WithCycle -> throw $ ReplicationPresentError "heurBodyDup (WithCycle)"
                  WithSoar -> throw $ ReplicationPresentError "heurBodyDup (WithSoar)"
            hd' <- initializeWithMaps scored_maps' hd
            void . runHeuristic $ hd'
            test_number <$> getScores

        heurBodyRep :: (MonadRandomRecorder MapOfRep mon', GmapGenerator gen MapOfRep, MapHeuristicData hd mon' MapOfRep) => gen -> hd -> mon' [Dist]
        heurBodyRep gen hd = do
            create <- createOnStart hd
            scored_maps' <-
                case withMap of
                  WithRand -> generateAndEvaluateMaps gen create
                  WithCycle -> do
                      scored_maps <- generateAndEvaluateMaps gen (create - 1)
                      scored_m_cycle <- evaluateMap m_cycle
                      return (scored_m_cycle:scored_maps)
                  WithSoar -> do
                      scored_maps <- generateAndEvaluateMaps gen (create - 1)
                      scored_m_soar <- evaluateMap m_soar
                      return (scored_m_soar:scored_maps)
            hd' <- initializeWithMaps scored_maps' hd
            void . runHeuristic $ hd'
            test_number <$> getScores

        test_number scores = if force_limit && length scores < createTotal
                                then throw $ TooFillMaps "findDistance"
                                else scores

    recorder :: (I.ReaInstGenome inst g a, GmapGenerator gen m) => inst -> gen -> Recorder m mon
    recorder inst gen = prepareRecorder (eval inst gen) createTotal

    eval :: (I.ReaInstGenome inst g a, GmapGenerator gen m, Gmap m) => inst -> gen -> (m -> mon Dist)
    eval inst gen = makeDistEstimator inst pd (mapGenome (standardMap gen) g2) . (`mapGenome` g1)
      where
        g1 = I.getSource inst
        g2 = I.getTarget inst

-- Select correct heuristic to use.
useHeurData :: (GmapGenerator gen m, MonadRandomDist mon, Move mov m, WithSep sg gen m, mon' ~ RecMon m mon) => MapHeur -> gen -> (gen -> Recorder m mon) -> (forall hd. (MapHeuristicData hd mon' m) => gen -> hd -> mon' a) -> mon a
useHeurData mh gen rec f = (`evalStateT` rec gen) $
  case mh of
    (A2 com) -> f gen (initializeMap com gen)
    (A4 com) -> f gen (initializeLocal com gen)
    (A7 com) -> f gen (initializeTabu com gen)
    (A14 com) -> f gen (initializeCuckoo com gen)
    (A5 com) -> f gen (initializeGrasp com gen)
    (A3 com) -> f gen =<< initializeGenAlg com gen
    (A15 com) -> f gen =<< initializeMemAlg com gen
    (A8 com) -> f gen (initializeSimAnn com gen)
    (A9 com) -> f gen =<< initializeSep com gen
    (BX com) -> f gen =<< initializeCuckooMeta com gen
    (CX com) -> f gen =<< initializeSepMeta com gen

