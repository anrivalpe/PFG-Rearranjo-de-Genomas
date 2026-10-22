{-# LANGUAGE TypeApplications, ScopedTypeVariables, FlexibleContexts, GADTs, TupleSections #-}

module HittingSetAux (generateT', addFirstAndLastDue, emptyHS, contractedRepresentation, getStrips, emptyIntersectHS) where

--------------------------------------------------
import Utils
import Types
import Aux
--------------------------------------------------
import Data.Set (Set)
--------------------------------------------------
import qualified Instance as I
import qualified Genome as G
--------------------------------------------------
import qualified Data.List as List
import qualified Data.SuffixTree as ST
import qualified Data.Set as Set
--------------------------------------------------

data HSSTree a = HSNode SumA SumB Proper [(ST.Prefix a, HSSTree a)] | HSLeaf SumA SumB Proper deriving (Show)
newtype HittingSet mod a = HittingSet (Set (Due mod a)) deriving (Show)
type SumA = Int
type SumB = Int
type Proper = Bool

emptyHS :: HittingSet mod a
emptyHS = HittingSet Set.empty

emptyIntersectHS :: forall mod a. (G.Gene a, Orientable (Due mod a)) => HittingSet mod a -> [a] -> Bool
emptyIntersectHS (HittingSet hs) = not . any ((`Set.member` hs) . canonicOri . toDue @mod) . lPairs

addFirstAndLastDue :: forall mod a. (G.Gene a, Orientable (Due mod a)) => HittingSet mod a -> [a] -> HittingSet mod a
addFirstAndLastDue (HittingSet hs) subStr = HittingSet . Set.insert firstDue . Set.insert lastDue $ hs
  where
    firstDue = canonicOri $ head dues
    lastDue = canonicOri $ last dues
    dues = map (toDue @mod) $ lPairs subStr

generateT' :: forall inst g a mod. (I.ReaInstGenMod inst g a mod) => inst -> [[a]]
generateT' inst = Set.toAscList $ getT' [] [] hSSTree
  where
    g1 = I.getSource inst
    g2 = I.getTarget inst
    l1 = G.toListOfGenes g1
    l2 = G.toListOfGenes g2
    rl1 = G.toListOfGenes . G.rev 1 (fromIntegral n) $ g1
    rl2 = G.toListOfGenes . G.rev 1 (fromIntegral n) $ g2
    n = fromIntegral . I.len $ inst
    str = if hasRev @mod
             then l1 ++ (n+3:rl1) ++ (n+1:l2) ++ (n+4:rl2) ++ [n+2]
             else l1 ++ (n+1:l2) ++ [n+2]
    markers = [n+1,n+2,n+3,n+4]

    getT' pfx s (HSLeaf sumA sumB proper) = if sumA /= sumB && proper
                                           then Set.singleton (reverse $ head pfx:s)
                                           else Set.empty
    getT' pfx s (HSNode sumA sumB proper edges) = if sumA /= sumB && proper && pfx /= []
                                                   then Set.insert (reverse $ head pfx:s) subT'
                                                   else subT'
      where subT' = Set.unions . map (\(p,subTree) -> getT' (ST.prefix p) (reverse pfx ++ s) subTree) $ edges

    hSSTree = makeHSSTree [0] False True (ST.construct str)
    makeHSSTree pfx inA noMarkers ST.Leaf =
      (if inA then HSLeaf 1 0 else HSLeaf 0 1) (noMarkers && head pfx `notElem` markers)
    makeHSSTree pfx inA noMarkers (ST.Node edges) = HSNode sumA sumB proper edges'
      where getA t = case t of {HSNode a _ _ _ -> a; HSLeaf a _ _ -> a}
            getB t = case t of {HSNode _ b _ _ -> b; HSLeaf _ b _ -> b}
            sumA = sum . map getA $ subTrees
            sumB = sum . map getB $ subTrees
            edges' = zip (fst <$> edges)  subTrees
            proper = noMarkers && head pfx `notElem` markers

            subTrees = makeSubHSSTree <$> edges
            makeSubHSSTree (p_,t) = let p = ST.prefix p_ in
              makeHSSTree p (inA || (n+1) `elem` p) noMarkers' t
            noMarkers' = noMarkers && null (pfx `List.intersect` markers)

contractedRepresentation :: forall inst g a a' mod. (I.ReaInstAll inst g a a' mod, Orientable (Strip mod a), Orientable (Due mod a)) => inst -> Bool -> HittingSet mod a -> (I.AnyInstance, BreakPoints)
contractedRepresentation inst asPerm = fst . contractedRepresentationAndStrips @mod inst asPerm

getStrips :: forall inst g a a' mod. (I.ReaInstAll inst g a a' mod, Orientable (Strip mod a), Orientable (Due mod a)) => inst -> Bool -> HittingSet mod a -> Strips mod a
getStrips inst asPerm = snd . contractedRepresentationAndStrips @mod inst asPerm

contractedRepresentationAndStrips ::
       forall mod inst g a a'.
       ( I.ReaInstAll inst g a a' mod
       , Orientable (Strip mod a)
       , Orientable (Due mod a))
    => inst
    -> Bool
    -> HittingSet mod a
    -> ((I.AnyInstance, BreakPoints), Strips mod a)
contractedRepresentationAndStrips inst asPerm hs =
    ((I.AnyInstance $ I.replaceGenomes g1' g2' inst, bps), strips)
      where
        bps = getBPFromHS inst hs
        strips = breaks2Strips l1 l2 bps
        (l1',l2') = contractedRepresentation' @mod l1 l2 n bps asPerm
        g1' = (G.fromListOfGenes l1') :: g a'
        g2' = (G.fromListOfGenes l2') :: g a'
        (l1, l2, n, _, _) = I.unwrapInstanceAsLists inst

getBPFromHS :: forall mod inst g a. (I.ReaInstGenome inst g a, Orientable (Due mod a)) => inst -> HittingSet mod a -> BreakPoints
getBPFromHS inst (HittingSet hs) = (partition l1, partition l2)
  where
    (l1, l2, n, _, _) = I.unwrapInstanceAsLists inst
    partition = map fst . filter (isBreakpoint . snd) . zip [1..fromIntegral n] . map (toDue @mod) . lPairs
      where
        isBreakpoint due = Set.member (canonicOri due) hs
