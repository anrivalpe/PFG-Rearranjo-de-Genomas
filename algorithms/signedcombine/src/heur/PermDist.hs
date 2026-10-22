{-# LANGUAGE FlexibleContexts, GADTs #-}

module PermDist (permDist, DistEstimator, makeDistEstimator) where

--------------------------------------------------
import Types
--------------------------------------------------
import Args (PermDist(..))
import Perm (MonadDist, PermData, Perm, findDistancePerm)
--------------------------------------------------
import qualified Instance as I
--------------------------------------------------

permDist :: (MonadDist mon) => PermDist -> (I.AnyInstance, PermData) -> mon Dist
permDist PermDist (I.AnyInstance inst,pd) = do
  let distEst = makeDistEstimator inst pd
  let (p1,p2,_) = I.unwrapInstanceAsPerms inst
  distEst p1 p2

type DistEstimator a mon = (Perm -> Perm -> mon Dist)

makeDistEstimator :: (I.ReaInstGenome inst g a, MonadDist mon) => inst -> PermData -> DistEstimator a mon
makeDistEstimator inst pd = findDistancePerm pd (I.len inst)
