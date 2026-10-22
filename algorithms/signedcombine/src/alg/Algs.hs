{-# LANGUAGE ExplicitForAll, FlexibleContexts, Rank2Types, TypeFamilies #-}

module Algs (hittingSet, soar, cycleDec, soarMap, cycleMap) where

--------------------------------------------------
import Types
--------------------------------------------------
import MCSP (ContractedInstance(..), mcspHS, mcspSOARWithBreak, assigments, useMapFromContracted)
import SoarAux (cicleDecomposition)
import Perm (MonadDist, PermData, findDistancePerm, Perm)
import Args (HS(..), Soar(..), Cycle(..))
import Mapping (recoverMap, combineMaps)
import MappingRep (MapOfRep, makeGeneratorRep)
--------------------------------------------------
import qualified Instance as I
import qualified ArgsMCSP as MCSP
--------------------------------------------------

hittingSet :: forall mon. (MonadDist mon) => HS -> (I.AnyInstance, PermData) -> mon Dist
hittingSet HS (I.AnyInstance inst,pd) = do
  (ContInst (I.AnyInstance inst') _) <- return $ mcspHS (MCSP.HS False) True inst
  let (perm1,perm2,n) = I.unwrapInstanceAsPerms inst'
  findDistancePerm pd n perm1 perm2

soar :: forall mon. (MonadDist mon) => Soar -> (I.AnyInstance, PermData) -> mon Dist
soar (Soar aprox noHeu useMap) (inst, pd) = findDistancePerm pd n perm1 perm2
  where
      inst' = (if noHeu then id else assigments) inst
      (perm1,perm2,n) = soarPermCustom (mcspSOARWithBreak (MCSP.Soar aprox False)) useMap inst'

cycleDec :: (MonadDist mon) => Cycle -> (I.AnyInstance, PermData) -> mon Dist
cycleDec Cycle (I.AnyInstance inst, pd) = findDistancePerm pd n perm1 perm2
    where
        (perm1,perm2,n) = I.unwrapInstanceAsPerms $ cicleDecomposition inst

soarMap :: Soar -> I.AnyInstance -> MapOfRep
soarMap (Soar aprox noHeu _) anyInst = getMap anyInst perms
  where
      inst' = (if noHeu then id else assigments) anyInst
      perms = soarPermCustom (mcspSOARWithBreak (MCSP.Soar aprox False)) True inst'

cycleMap :: I.AnyInstance -> MapOfRep
cycleMap anyInst@(I.AnyInstance inst) = getMap anyInst perms
  where
    inst' = cicleDecomposition inst
    perms = I.unwrapInstanceAsPerms inst'

getMap :: I.AnyInstance -> (Perm,Perm,Size) -> MapOfRep
getMap (I.AnyInstance inst) (perm1,perm2,_) =
  combineMaps gen (recoverMap perm1 g1) (recoverMap perm2 g2)
    where
      gen = makeGeneratorRep $ I.getRepInfo inst
      g1 = I.getSource inst
      g2 = I.getTarget inst

soarPermCustom :: (forall inst g a a' mod. (I.ReaInstAll inst g a a' mod) => inst -> ContractedInstance) -> Bool -> I.AnyInstance -> (Perm,Perm,Size)
soarPermCustom customMCSP useMap (I.AnyInstance inst) =
  getPermWithCicle $ customMCSP inst
    where
      getPermWithCicle (ContInst (I.AnyInstance conInst) bps) =
        let conInst' = cicleDecomposition conInst in
        if useMap
           then I.unwrapInstanceAsPerms $ useMapFromContracted conInst' inst bps
           else I.unwrapInstanceAsPerms conInst'
