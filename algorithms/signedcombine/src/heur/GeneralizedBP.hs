{-# LANGUAGE FlexibleContexts, GADTs, TupleSections, TypeApplications, ExplicitForAll, ScopedTypeVariables #-}

module GeneralizedBP (generalizedBP, buildSBPCount, incSBPCount, decSBPCount, getSBPCount) where

--------------------------------------------------
import Utils
import Types
import Aux
--------------------------------------------------
import Extremities (candidates, rank)
import Perm (MonadDist, PermData, findDistancePerm)
import Args (GenBP(..))
--------------------------------------------------
import Control.Applicative ((<|>))
import Control.Arrow (first)
import Control.Exception.Base (throw)
import Data.Maybe (fromJust, fromMaybe, listToMaybe)
import Data.Ord (comparing)
--------------------------------------------------
import qualified Instance as I
import qualified Genome as G
--------------------------------------------------
import qualified Data.List as List
--------------------------------------------------

generalizedBP :: (MonadDist mon) => GenBP -> (I.AnyInstance, PermData) -> mon Dist
generalizedBP (GenBP trans_first ext) (I.AnyInstance inst_,pd) = do
    let inst = I.makeExtended inst_
        sbpCount = buildSBPCount inst
        inst' = case getModel inst of 
                  Rev -> generalizedBPLoopRev
                  Trans -> generalizedBPLoopTrans
                  TransRev -> generalizedBPLoopTransRev
                $ (inst,sbpCount)
    dist <- if I.sameGenomes inst'
                then return 0
                else let (perm1,perm2,n) = I.withoutExtencion I.unwrapInstanceAsPerms inst' in
                  findDistancePerm pd n perm1 perm2
    return (I.numOp inst' + dist)
  where
    getModel :: forall inst g a mod. (I.ReaInstGenMod inst g a mod) => inst -> RearrangeModel
    getModel _ = model @mod

    generalizedBPLoopRev (inst', gbpd) =
      let cutted = cutEdgesLeaveExt inst' in
      if I.isPerm cutted || I.sameGenomes cutted
      then cutted
      else generalizedBPLoopRev . fromJust $
           removeTwoBPWithRev cutted gbpd <|>
           removeOneBPWithRev cutted gbpd <|>
           Just (placeExtremityWithRevGBP ext cutted gbpd)

    generalizedBPLoopTrans (inst', gbpd) =
      let cutted = cutEdgesLeaveExt inst' in
      if I.isPerm cutted || I.sameGenomes cutted
      then cutted
      else generalizedBPLoopTrans . fromJust $
           removeThreeBPWithTrans cutted gbpd <|>
           removeTwoBPWithTrans cutted gbpd <|>
           removeOneBPWithTrans cutted gbpd <|>
           Just (placeExtremityWithTransGBP ext cutted gbpd)

    generalizedBPLoopTransRev (inst', gbpd) =
      let cutted = cutEdgesLeaveExt inst' in
      if I.isPerm cutted || I.sameGenomes cutted
      then cutted
      else generalizedBPLoopTransRev . fromJust $
           removeThreeBPWithTrans cutted gbpd <|>
           (if trans_first then
             removeTwoBPWithTrans cutted gbpd <|>
             removeTwoBPWithRev cutted gbpd <|>
             removeOneBPWithTrans cutted gbpd <|>
             removeOneBPWithRev cutted gbpd
           else
             removeTwoBPWithRev cutted gbpd <|>
             removeTwoBPWithTrans cutted gbpd <|>
             removeOneBPWithRev cutted gbpd <|>
             removeOneBPWithTrans cutted gbpd) <|>
           Just (placeExtremityWithTransRevGBP ext cutted gbpd)

cutEdgesLeaveExt :: (I.ReaInstGenome inst g a) => inst -> inst
cutEdgesLeaveExt inst = I.cut prefix suffix inst
  where
    g1 = I.getSource inst
    g2 = I.getTarget inst
    prefix = G.commumPrefix g1 g2 - 1
    suffix = G.commumSuffix g1 g2 - 1

data BPNumR = BPR1 | BPR2 deriving (Show)
data BPNumT = BPT1 | BPT2 | BPT3 deriving (Show)

getBpsAndPoss :: forall mod inst g a. (I.ReaInstGenMod inst g a mod) => Bool -> inst -> StrBreakPointCount mod a -> (g a, [(Idx,Due mod a)], G.PositionsOri a)
getBpsAndPoss isRev inst sbpCount = (g1,bps,poss)
  where
    g1 = I.getSource inst
    n = fromIntegral . I.len $ inst
    dues = map (toDue @mod) . lPairs . G.toListOfGenes $ g1
    bps =  filter ((> 0) . getSBPCount sbpCount . snd) . zip [2..(if isRev then n-1 else n)] $ dues
    poss = G.posAS g1

removeTwoBPWithRev :: (I.ExtendedInstance inst, I.ReaInstGenMod inst g a mod, c ~ StrBreakPointCount mod a) => inst -> c -> Maybe (inst, c)
removeTwoBPWithRev = removeBPWithRev BPR2

removeOneBPWithRev :: (I.ExtendedInstance inst, I.ReaInstGenMod inst g a mod, c ~ StrBreakPointCount mod a) => inst -> c -> Maybe (inst, c)
removeOneBPWithRev = removeBPWithRev BPR1

removeBPWithRev :: forall inst g a mod c. (I.ExtendedInstance inst, I.ReaInstGenMod inst g a mod, c ~ StrBreakPointCount mod a) => BPNumR -> inst -> c -> Maybe (inst, c)
removeBPWithRev bpNum inst sbpCount = do
  idx <- listToMaybe . concatMap findRev $ bps
  return $ revAndUpdateSBPCount idx inst sbpCount
  where
    (g1,bps,poss) = getBpsAndPoss True inst sbpCount
    n = I.len inst

    findRev (i,Due a1 a2) = map (R i) secondsR ++ map (`R` (i-1)) secondsL
      where
        secondsR = filter checkRevR $ findSecond True a1
        secondsL = filter checkRevL $ findSecond False a2
        checkRevR j = (G.sig g1 || i /= j) &&
            case bpNum of
              BPR1 ->
                getSBPCount sbpCount (toDue @mod (invOri a2, b2)) < 0 ||
                getSBPCount sbpCount (toDue @mod (b1, b2)) > 0
              BPR2 ->
                getSBPCount sbpCount (toDue @mod (invOri a2, b2)) < 0 &&
                getSBPCount sbpCount (toDue @mod (b1, b2)) > 0
          where
            b1 = G.ele g1 j
            b2 = G.ele g1 (j+1)

        checkRevL j =
          case bpNum of
            BPR1 -> getSBPCount sbpCount (toDue @mod (invOri b1,a1)) < 0 ||
                getSBPCount sbpCount (toDue @mod (b1,b2)) > 0
            BPR2 -> getSBPCount sbpCount (toDue @mod (invOri b1,a1)) < 0 &&
                getSBPCount sbpCount (toDue @mod (b1,b2)) > 0
          where
            b1 = G.ele g1 (j-1)
            b2 = G.ele g1 j

        findSecond toRight = filter checkIdx . concatMap getJ . negativeList
          where checkIdx j = if toRight
                             then i <= j && j < fromIntegral n
                             else 1 < j && j < i
                getJ = G.getPosOri poss . invOri . (\(Due a1 a2) -> if toRight then a2 else a1)
                negativeList = listNegativeSBPCount sbpCount toRight

revAndUpdateSBPCount :: (I.ExtendedInstance inst, I.ReaInstGenMod inst g a mod, c ~ StrBreakPointCount mod a) => RevOp -> inst -> c -> (inst, c)
revAndUpdateSBPCount idxs@(R i j) inst sbpCount = (inst',gbpd')
    where
      g1 = I.getSource inst
      inst' = I.rev idxs inst
      a1 = G.ele g1 (i-1)
      a2 = G.ele g1 i
      b1 = G.ele g1 j
      b2 = G.ele g1 (j+1)
      gbpd' = decSBPCount (Due a1 a2) . decSBPCount (Due b1 b2) .
              incSBPCount (Due a1 (invOri b1)) . incSBPCount (Due (invOri a2) b2) $ sbpCount

placeExtremityWithRevGBP :: (I.ExtendedInstance inst, I.ReaInstGenMod inst g a mod, c ~ StrBreakPointCount mod a) => Bool -> inst -> c -> (inst, c)
placeExtremityWithRevGBP ext inst sbpCount
  | ext =
      applyRevs (inst, sbpCount) .
      map (translateRop 1) . fst . List.maximumBy (comparing snd) .
        I.withoutExtencion (\instNoExt -> ranksPref instNoExt ++ ranksSuff instNoExt) $ inst
  | I.withoutExtencion (\instNoExt' ->
    G.beg (I.getSource instNoExt') == G.beg (I.getTarget instNoExt')) inst' =
      (inst', sbpCount')
  | otherwise = revAndUpdateSBPCount (R 2 2) inst' sbpCount'
  where
    j = case listToMaybe . I.withoutExtencion candidates $ inst of
          Nothing -> error patternError
          Just j -> j+1
    (inst',sbpCount') = revAndUpdateSBPCount (R 2 j) inst sbpCount
    ranksSuff inst_ = map (first . map $ mirrorRop (I.len inst_)) . ranksPref . I.mirror $ inst_
    ranksPref inst_ = map (rank inst_) $ oneRev inst_
    oneRev = map (\j -> [R 1 j]) . candidates
    applyRevs = foldl (\(inst_,gbpd_) idxs -> revAndUpdateSBPCount idxs inst_ gbpd_)

removeThreeBPWithTrans :: (I.ExtendedInstance inst, I.ReaInstGenMod inst g a mod, c ~ StrBreakPointCount mod a) => inst -> c -> Maybe (inst, c)
removeThreeBPWithTrans = removeBPWithTrans BPT2

removeTwoBPWithTrans :: (I.ExtendedInstance inst, I.ReaInstGenMod inst g a mod, c ~ StrBreakPointCount mod a) => inst -> c -> Maybe (inst, c)
removeTwoBPWithTrans = removeBPWithTrans BPT2

removeOneBPWithTrans :: (I.ExtendedInstance inst, I.ReaInstGenMod inst g a mod, c ~ StrBreakPointCount mod a) => inst -> c -> Maybe (inst, c)
removeOneBPWithTrans = removeBPWithTrans BPT1

removeBPWithTrans :: forall inst g a mod c. (I.ExtendedInstance inst, I.ReaInstGenMod inst g a mod, c ~ StrBreakPointCount mod a) => BPNumT -> inst -> c -> Maybe (inst, c)
removeBPWithTrans bpNum inst sbpCount = do
  idx <- listToMaybe . concatMap findTrans $ bps
  return $ transAndUpdateSBPCount idx inst sbpCount
  where
    (g1,bps,poss) = getBpsAndPoss False inst sbpCount
    n = fromIntegral $ I.len inst

    checkIdxs (Due a1 a2, Due b1 b2, Due c1 c2) =
      case bpNum of
        BPT1 -> bps <= -4
        BPT2 -> bps <= -5
        BPT3 -> bps == -6
      where
        l_due = [Due a1 a2, Due b1 b2, Due c1 c2, Due a1 b2, Due c1 a2, Due b1 c2]
        l_dec = [False,False,False,True,True,True]
        l = zip l_due l_dec
        bps = sum . snd . List.mapAccumL testBP sbpCount $ l

    testBP sbpCount (due,inc) =
      if inc
       then (incSBPCount due sbpCount, to1 . getSBPCount sbpCount $ due)
       else (decSBPCount due sbpCount, negate . to1 . getSBPCount sbpCount $ due)
    to1 v | v < 0 = -1 | v == 0 = 0 | otherwise = 1 -- v > 0

    findNext l r starting (Due a1 a2) = filter checkIdx . concatMap getIdx $ negativeList
      where checkIdx idx = l < idx && idx < r
            getIdx (Due a1' a2') =
              if starting
                then G.getPosOri poss $ a2'
                else map (+ 1) . G.getPosOri poss $ a1'
            negativeList = listNegativeSBPCount sbpCount starting $ if starting then a1 else a2

    findTrans (i,due_i) = idx_l ++ idx_m ++ idx_r
      where
        idx_l =
          let js1 = findNext i (n+1) True due_i
              idxs1 = concatMap (\j -> findK j (n+1) True j) js1
              js2 = findNext i (n+1) False due_i
              idxs2 = concatMap (\j -> findK i j False j) js2
          in idxs1 ++ idxs2

        idx_m =
          let js1 = findNext 1 i False due_i
              idxs1 = concatMap (findK i (n+1) False) js1
              js2 = findNext i (n+1) True due_i
              idxs2 = concatMap (findK 1 i True) js2
          in idxs1 ++ idxs2

        idx_r =
          let js1 = findNext 1 i False due_i
              idxs1 = concatMap (\j -> findK 1 j False j) js1
              js2 = findNext 1 i True due_i
              idxs2 = concatMap (\j -> findK j i True j) js2
          in idxs1 ++ idxs2

        findK l r starting j =
          let due_j = getDue j
              ks = case bpNum of
                      BPT1 -> [(l+1)..(r-1)]
                      _ -> findNext l r starting due_j
          in map fst . filter (checkIdxs . snd) . map (correctOrder i j due_i due_j) $ ks

        correctOrder i j due_i due_j k
          | i < j && j < k = (T i j k,(due_i,due_j,due_k))
          | j < i && i < k = (T j i k,(due_j,due_i,due_k))
          | i < k && k < j = (T i k j,(due_i,due_k,due_j))
          | j < k && k < i = (T j k i,(due_j,due_k,due_i))
          | k < i && i < j = (T k i j,(due_k,due_i,due_j))
          -- k < j && j < i
          | otherwise = (T k j i,(due_k,due_j,due_i))
          where due_k = getDue k

        getDue l = toDue @mod (G.ele g1 (l-1), G.ele g1 l)

transAndUpdateSBPCount :: (I.ExtendedInstance inst, I.ReaInstGenMod inst g a mod, c ~ StrBreakPointCount mod a) =>
  TransOp -> inst -> c -> (inst, c)
transAndUpdateSBPCount idxs@(T i j k) inst sbpCount = (inst',sbpCount')
    where
      g1 = I.getSource inst
      inst' = I.trans idxs inst
      a1 = G.ele g1 (i-1)
      a2 = G.ele g1 i
      b1 = G.ele g1 (j-1)
      b2 = G.ele g1 j
      c1 = G.ele g1 (k-1)
      c2 = G.ele g1 k
      sbpCount' = decSBPCount (Due a1 a2) . decSBPCount (Due b1 b2) . decSBPCount (Due c1 c2) .
                  incSBPCount (Due a1 b2) . incSBPCount (Due c1 a2) . incSBPCount (Due b1 c2) $
                      sbpCount

placeExtremityWithTransGBP :: (I.ExtendedInstance inst, I.ReaInstGenMod inst g a mod, c ~ StrBreakPointCount mod a) => Bool -> inst -> c -> (inst, c)
placeExtremityWithTransGBP ext inst gbpd
  | I.sig inst = throw $ GenomesWithSign "placeExtremityWithTransGBP"
  | ext = undefined
  | otherwise = transAndUpdateSBPCount (T 2 j n) inst gbpd
  where
    j = case listToMaybe . I.withoutExtencion candidates $ inst of
              Nothing -> error patternError
              Just j -> j+1
    n = fromIntegral $ I.len inst

placeExtremityWithTransRevGBP :: (I.ExtendedInstance inst, I.ReaInstGenMod inst g a mod, c ~ StrBreakPointCount mod a) => Bool -> inst -> c -> (inst, c)
placeExtremityWithTransRevGBP ext inst gbpd
  | ext = undefined
  | I.withoutExtencion (\instNoExt -> a == G.beg (I.getTarget instNoExt)) inst =
    transAndUpdateSBPCount (T 2 j n) inst gbpd
  | otherwise = revAndUpdateSBPCount (R 2 j) inst gbpd
  where
    j = case listToMaybe . I.withoutExtencion candidates $ inst of
          Nothing -> error patternError
          Just j -> j+1
    n = fromIntegral $ I.len inst
    a = G.ele (I.getSource inst) j

-------------------- Break Points (Strings) --------------------------

data StrBreakPointCount mod a = SBPCount (DueMap mod Int) (DueMap mod Int) deriving (Show)

buildSBPCount :: forall mod inst g a. (I.ReaInstGenMod inst g a mod) => inst -> StrBreakPointCount mod a
buildSBPCount inst = SBPCount (dueMap LR) (dueMap RL)
  where
    g1 = I.getSource inst
    g2 = I.getTarget inst
    dueMap ori = toDueMap ori (+) (dueFreqMap 1 g1 ++ dueFreqMap (-1) g2)
    dueFreqMap e = map ((,e) . toDue @mod) . lPairs . G.toListOfGenes

getSBPCount :: (G.Gene a, Model mod) => StrBreakPointCount mod a -> Due mod a -> Int
getSBPCount (SBPCount dueMap _) due = fromMaybe 0 $ dueMapFind dueMap due

incSBPCount :: (G.Gene a, Model mod) => Due mod a -> StrBreakPointCount mod a -> StrBreakPointCount mod a
incSBPCount = changeSBPCount 1

decSBPCount :: (G.Gene a, Model mod) => Due mod a -> StrBreakPointCount mod a -> StrBreakPointCount mod a
decSBPCount = changeSBPCount (-1)

changeSBPCount :: (G.Gene a, Model mod, c ~ StrBreakPointCount mod a) => Int -> Due mod a -> c -> c
changeSBPCount e due (SBPCount dueMap dueMapRev) = SBPCount dueMap' dueMapRev'
  where
    dueMap' = dueMapAlter incFun due dueMap
    dueMapRev' = dueMapAlter incFun due dueMapRev
    incFun Nothing = e
    incFun (Just val) = val + e

listNegativeSBPCount :: forall mod a. (G.Gene a, Model mod) => StrBreakPointCount mod a -> Bool -> a -> [Due mod a]
-- Dues with negative values and starting\ending with gene a.
listNegativeSBPCount (SBPCount dueMap dueMapRev) starting a =
  if starting
  then dirDues_fst ++ case model @mod of
    Trans -> []
    Rev -> revDues_fst
    TransRev -> revDues_fst
  else dirDues_snd ++ case model @mod of
    Trans -> []
    Rev -> revDues_snd
    TransRev -> revDues_snd
  where
    dirDues_fst = dueMapFindDuesWithGene dueMap a (< 0)
    revDues_fst = invOri <$> dueMapFindDuesWithGene dueMapRev (invOri a) (< 0)
    dirDues_snd = dueMapFindDuesWithGene dueMapRev a (< 0)
    revDues_snd = invOri <$> dueMapFindDuesWithGene dueMap (invOri a) (< 0)

