{-# LANGUAGE FlexibleContexts, GADTs #-}

module Extremities (extremities, candidates, rank, cutEdges) where

--------------------------------------------------
import Types
--------------------------------------------------
import Perm (MonadDist, PermData, findDistancePerm)
import Args (Ext(..))
import Aux (RevOp(..), mirrorRop)
--------------------------------------------------
import Control.Arrow (first)
import Data.Ord (comparing)
--------------------------------------------------
import qualified Instance as I
import qualified Genome as G
--------------------------------------------------
import qualified Data.List as List
--------------------------------------------------

extremities :: (MonadDist mon) => Ext -> (I.AnyInstance, PermData) -> mon Dist
extremities (Ext use_all) (I.AnyInstance inst,pd) = do
    inst' <- extremitiesLoop inst
    dist <- if I.empty inst'
                then return 0
                else let (perm1,perm2,n) =
                           I.unwrapInstanceAsPerms inst' in
                           findDistancePerm pd n perm1 perm2
    return (I.numOp inst' + dist)
  where
    extremitiesLoop inst' = do
      let cutted = cutEdges inst'
      if I.isPerm cutted
      then return cutted
      else extremitiesLoop (placeExtremity use_all cutted)

cutEdges :: (I.ReaInstGenome inst g a) => inst -> inst
cutEdges inst = I.cut prefix suffix inst
  where
    g1 = I.getSource inst
    g2 = I.getTarget inst
    prefix = G.commumPrefix g1 g2
    suffix = G.commumSuffix g1 g2

candidates :: (I.ReaInstGenome inst g a) => inst -> [Idx]
candidates inst = G.pos g1 beg
  where
    g1 = I.getSource inst
    beg = G.beg . I.getTarget $ inst

rank :: (I.ReaInstGenome inst g a) => inst -> [RevOp] -> ([RevOp], Double)
rank inst revs = if G.beg g1' == beg
                    then (revs, fromIntegral $ G.commumPrefix g1' g2 * (6 `div` nop))
                    else let g1'' = I.getSource . I.rev (R 1 1) $ inst' in
                        (revs ++ [R 1 1], fromIntegral $ G.commumPrefix g1'' g2 * (6 `div` (nop+1)))
  where
    inst' = List.foldl' (flip I.rev) inst revs
    g1' = I.getSource inst'
    g2 = I.getTarget inst
    beg = G.beg g2
    nop = length revs

placeExtremity :: (I.ReaInstGenome inst g a) => Bool -> inst -> inst
placeExtremity use_all inst = applyRevs . fst . List.maximumBy (comparing snd) $ ranksPref inst ++ ranksSuff inst
  where
    n = I.len inst
    applyRevs = List.foldl' (flip I.rev) inst
    ranksSuff = map (first . map $ mirrorRop n) . ranksPref . I.mirror
    ranksPref inst_ = map (rank inst_) $ oneRev inst_ ++ twoRev inst_
    oneRev = map (\j -> [R 1 j]) . candidates
    twoRev inst_ =
      if use_all
      then allRevs inst_
      else undefined
    allRevs inst_ = [let k' = if i <= k && k <= j then j - k + i else k in [R i j, R 1 k']
              | i <- [1..fromIntegral n], j <- [i..fromIntegral n], k <- candidates inst_]
