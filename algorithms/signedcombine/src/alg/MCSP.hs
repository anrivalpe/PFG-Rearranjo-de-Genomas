{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, GADTs, TypeApplications, TupleSections, UndecidableInstances, MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}

module MCSP (ContractedInstance(..), mcspHS, mcspSOAR, mcspSOARWithBreak, mcspCombine, mcspGreedy, assigments, ContractedData(..), contractMap, uncontractMap, ContractableGenerator(ContGen), useMapFromContracted) where

--------------------------------------------------
import Utils
import Types
import Aux
--------------------------------------------------
import HittingSetAux (generateT', addFirstAndLastDue, emptyHS, contractedRepresentation, emptyIntersectHS, getStrips)
import SoarAux (PMGraph(..), optimalAssigment, subOptimalAssigment1, subOptimalAssigment2, contractedRepresentation, makePMGraph, getBPFromIS, PMGraph3, PMGraph4)
import ArgsMCSP (Soar(..), HS(..), Combine(..), Greedy(..))
import MappingRep (makeGeneratorRep)
import Mapping (Gmap(..), GmapGenerator(..), SepGen, ConstGen)
--------------------------------------------------
import Control.Arrow ((***))
import Control.Exception.Base (throw)
import Data.Foldable (toList)
import Data.Maybe (fromJust, listToMaybe, catMaybes)
import Data.Proxy (Proxy(..))
import Data.Sequence (Seq(..), (><))
--------------------------------------------------
import qualified Data.Sequence as Seq
import qualified Data.List as List
import qualified Data.IntMap as IntMap
import qualified Data.SuffixTree as T
--------------------------------------------------
import qualified Instance as I
import qualified Genome as G
--------------------------------------------------

--------------------------------------- MCSP algorithms --------------------------------

data ContractedInstance = ContInst
    { contractedInst :: I.AnyInstance
    , breakpoints :: BreakPoints }

mcspHS :: forall inst g a a' mod. (I.ReaInstAll inst g a a' mod) => HS -> Bool -> inst -> ContractedInstance
mcspHS (HS useComb) asPerm inst =
    if useComb
       then combine inst strips
       else (\(inst,bps) -> ContInst inst bps) $ HittingSetAux.contractedRepresentation inst asPerm hs
    where
      t' = generateT' inst
      hs = List.foldl' updateHS emptyHS t'
      updateHS hs subStr =
          if emptyIntersectHS hs subStr
              then addFirstAndLastDue hs subStr
              else hs
      strips = getStrips inst asPerm hs

mcspSOAR :: forall inst g a a' mod. (I.ReaInstAll inst g a a' mod) => Soar -> inst -> I.AnyInstance
mcspSOAR com = contractedInst . mcspSOARWithBreak com

mcspSOARWithBreak :: forall inst g a a' mod. (I.ReaInstAll inst g a a' mod) => Soar -> inst -> ContractedInstance
mcspSOARWithBreak (Soar Ap3 useComb) inst =
    if not (I.onlyDup inst)
       then throw $ ReplicationPresentError "soar"
       else mcspSOAR' useComb (Proxy @PMGraph3) inst
mcspSOARWithBreak (Soar Ap4 useComb) inst = mcspSOAR' useComb (Proxy @PMGraph4) inst

mcspSOAR' :: (I.ReaInstAll inst g a a' mod, PMGraph pmg mod) => Bool -> Proxy pmg -> inst -> ContractedInstance
mcspSOAR' useComb proxyP inst =
    if useComb
       then combine inst (breaks2Strips l1 l2 bps)
       else ContInst conInst bps
    where
      graph = makePMGraph proxyP inst
      bps = getBPFromIS inst (getPM graph) $ independentSet graph
      conInst = SoarAux.contractedRepresentation bps False inst
      (l1,l2,_,_,_) = I.unwrapInstanceAsLists inst

data IdxEl a = IdxEl
    { ipos :: Int
    , ilabel :: a}
instance (Show a) => Show (IdxEl a) where
    show (IdxEl _ a) = show a
instance (Eq a) => Eq (IdxEl a) where
  idxEl1 == idxEl2 = ilabel idxEl1 == ilabel idxEl2
instance (Ord a) => Ord (IdxEl a) where
  idxEl1 >= idxEl2 = ilabel idxEl1 >= ilabel idxEl2
  idxEl1 `compare` idxEl2 = ilabel idxEl1 `compare` ilabel idxEl2
instance (Orientable a) => Orientable (IdxEl a) where
  getOri = getOri . ilabel
  invOri idxEl = IdxEl (ipos idxEl) (invOri . ilabel $ idxEl)

data Block a a' = Block
    { bidx :: Int
    , bstrip :: Seq a
    , blabel :: a'
    , bsingleton :: Bool
    } deriving (Show)
instance (Eq a') => Eq (Block a a') where
  b1 == b2 = blabel b1 == blabel b2
data CombineWhat = CombineSinSin | CombineSinRep | CombineRepRep

mcspCombine :: forall inst g a a' mod. (I.ReaInstAll inst g a a' mod) => Combine -> inst -> ContractedInstance
mcspCombine Combine inst = combine inst (map (\a -> toStrip @mod [a]) l1, map (\a -> toStrip [a]) l2)
  where
      (l1,l2,_,_,_) = I.unwrapInstanceAsLists inst

combine :: forall a' inst g a mod. (I.ReaInstAll inst g a a' mod) => inst -> Strips mod a -> ContractedInstance
combine inst (ls1, ls2) = ContInst conInst bps
    where
        conBlocks = combine_ CombineSinSin acc_ (Seq.fromList bs1_, Seq.fromList bs2_)
        (l1',l2') = (fromBlocks *** fromBlocks) conBlocks
        g1' = G.renumber (G.fromListOfGenes . map fromIntegral $ l1') :: g a'
        g2' = G.renumber (G.fromListOfGenes . map fromIntegral $ l2') :: g a'
        conInst = I.AnyInstance $ I.replaceGenomes g1' g2' inst

        bps = (getBreak *** getBreak) conBlocks
        getBreak = tail . toList . fmap (Idx . bidx)

        n = I.len inst
        r = I.rep inst
        bs1_ = toBlock ls1 0
        bs2_ = toBlock ls2 0
        acc_ = (stm_, orig_reps, fromIntegral count_)
        (stm_,count_) = stripsToMap (fromIntegral n) (ls1,ls2)
        orig_reps = IntMap.fromListWith (+) . toList .
            fmap ((, 1 :: Int) . abs . fromIntegral . blabel) $ bs1_

        fromBlocks = toList . fmap blabel
        toBlock :: [Strip mod a] -> Int -> [Block a a']
        toBlock [] _ = []
        toBlock (s:ss) i = (Block i (Seq.fromList l) (fromJust $ stmOrientedLookup s stm_) (any (I.singleton r) l)) : toBlock ss (i + length l)
            where l = fromStrip s

        combine_ what acc_ (bs1, bs2) =
          if bs1' /= bs1
              then combine_ what acc' (bs1', bs2')
              else case what of
                       CombineSinSin -> combine_ CombineSinRep acc' (bs1', bs2')
                       CombineSinRep -> combine_ CombineRepRep acc' (bs1', bs2')
                       CombineRepRep -> (bs1', bs2')
          where
            (bs1',bs2',acc') = go1 acc_ (Empty, bs1, Empty, bs2)
            go1 acc (Empty,Empty,Empty,Empty) = (Empty,Empty,acc)
            go1 acc (pre,ba1:<|Empty,Empty,bs2) = (pre:|>ba1,bs2,acc)
            go1 acc (pre1,ba1:<|bb1:<|suf1,pre2,ba2:<|Empty) =
              go1 acc (pre1:|>ba1,bb1:<|suf1,Empty,pre2:|>ba2)
            go1 acc@(stm,reps,count) (pre1,bs1@(ba1:<|bb1:<|suf1),pre2,bs2@(ba2:<|bb2:<|suf2))
              | not testReps = go1 acc (pre1:|>ba1,bb1:<|suf1,Empty,pre2 >< bs2)
              | due1 == due2 = go1 (stm',reps',count') (pre1,b1':<|suf1,Empty,(pre2:|>b2') >< suf2)
              | otherwise = go1 acc (pre1,bs1,pre2:|>ba2,bb2:<|suf2)
              where
                reps' =
                  IntMap.alter decrementRepetition (abs . fromIntegral $ a1) .
                  IntMap.alter decrementRepetition (abs . fromIntegral $ b1) .
                  IntMap.alter incrementRepetition (abs . fromIntegral $ a1') $ reps
                incrementRepetition Nothing = Just 1
                incrementRepetition (Just x) = Just $ x + 1
                decrementRepetition Nothing = error patternError
                decrementRepetition (Just x)
                  | x == 1 = Nothing
                  | x > 1 = Just $ x - 1
                  | otherwise = error patternError
                testReps = case what of
                             CombineSinSin -> bsingleton ba1 && bsingleton bb1
                             CombineSinRep -> bsingleton ba1 || bsingleton bb1
                             CombineRepRep -> True
                b1' = Block i1 s1 a1' (isSina1 || isSinb1)
                b2' = Block i2 s2 a2' (isSina2 || isSinb2)
                (Block i1 sa1 a1 isSina1) = ba1
                (Block i2 sa2 a2 isSina2) = ba2
                (Block _ sb1 b1 isSinb1) = bb1
                (Block _ sb2 b2 isSinb2) = bb2
                due1 = canonicOri $ toDue @mod (a1,b1)
                due2 = canonicOri $ toDue @mod (a2,b2)
                s1 = sa1 >< sb1
                s2 = sa2 >< sb2
                count' = if keepOld then count else count+1
                (stm', a1', keepOld) = stmLookupInsert (toStrip @mod s1) count stm
                a2' = if s1 == s2 then a1' else invOri a1'
            go1 _ (_,_,_,_) = error patternError

mcspGreedy :: forall inst g a a' mod. (I.ReaInstAll inst g a a' mod) => Greedy -> inst -> ContractedInstance
mcspGreedy (Greedy withSin_) inst = ContInst conInst bps
    where
        (l1,l2,n,_,_) = I.unwrapInstanceAsLists inst
        zl1 = zip [1..] l1
        zl2 = zip [1..] l2
        il1 = map (\(i,a) -> IdxEl i a) zl1
        il2 = map (\(i,a) -> IdxEl i a) zl2

        -- Extract only the breakpoints
        bps = (cleanBreaks *** cleanBreaks) $ getBreaks (orig_reps,[il1],[il2],([],[]))
        -- Initial Map with number of repetitions for each gene
        orig_reps = IntMap.fromListWith (+) . map ((, 1 :: Int) . abs . fromIntegral) $ l1
        -- Get contracted Representation form breakpoints
        conInst = SoarAux.contractedRepresentation bps False inst
        -- Remove extremities that are not breaks
        cleanBreaks = List.delete (fromIntegral n) . List.delete 0 . uniqueSort
        -- Reverse respecting model
        revStrip' = map (\(IdxEl i a) -> IdxEl (ajustIdx i) a) . fromStrip . invOri . toStrip @mod
        ajustIdx i = case model @mod of
                     Rev -> negate i
                     TransRev -> negate i
                     Trans -> i

        -- Find places to break the original genomes, separating it in blocks
        getBreaks :: (IntMap.IntMap Int, [[IdxEl a]], [[IdxEl a]], BreakPoints) -> BreakPoints
        getBreaks (_,[],[],bps) = bps
        getBreaks (_,[],_,_) = error patternError
        getBreaks (_,_,[],_) = error patternError
        getBreaks (reps,ls1, ls2, (bps1, bps2)) = getBreaks . snd . maxWith fst . catMaybes $ do
            let (withSin,sin) = case listToMaybe . filter (\(_,v) -> v==1) . IntMap.assocs $ reps of
                                 -- In case there is no singleton proceed as in the original Greedy for this iteration
                                 Nothing -> (False, IdxEl 0 0)
                                 -- Otherwise choose the first singleton (only used with withSin)
                                 Just (a,_) -> (withSin_, IdxEl 0 (fromIntegral a))
            pos1s <- [1..length ls1] -- non deterministic position in ls1
            pos2s <- [1..length ls2] -- non deterministic position in ls2
            let (pref1s, l1:suf1s) = List.splitAt (pos1s - 1) ls1 -- get list in position pos1s
                (pref2s, l2_:suf2s) = List.splitAt (pos2s - 1) ls2 -- get list in position pos2s
            l2 <- [l2_,revStrip' l2_] -- non deterministically test l2 and reverse of l2
            if (withSin && not (sin `elem` l1 && sin `elem` l2 || invOri sin `elem` l1 && invOri sin `elem` l2))
            then return Nothing -- with withSin we must have sin in both lists
            else 
                let l = longestSubstring withSin sin l1 l2
                    Just pos1 = findSublistIndex l l1
                    Just pos2 = findSublistIndex l l2
                    (pref1, l1_@(IdxEl i11 _:_)) = List.splitAt pos1 l1
                    (suf1,i1s) = case drop n l1_ of
                                   [] -> ([],[Idx i11])
                                   suf1@(IdxEl i12 _:_) -> (suf1,[Idx i11, Idx i12])
                    (pref2, l2_@(IdxEl i21 _:_)) = List.splitAt pos2 l2
                    (suf2,i2s) = case drop n l2_ of
                                   [] -> ([],[Idx i21])
                                   suf2@(IdxEl i22 _:_) -> (suf2,[Idx i21, Idx i22])
                    n = length l
                    ls1' = pref1s ++ (filter (not . null) [pref1,suf1]) ++ suf1s
                    ls2' = pref2s ++ (filter (not . null) [pref2,suf2]) ++ suf2s
                    fixIdx i = if i < 0 then abs i else i - 1
                    i1s' = map fixIdx i1s
                    i2s' = map fixIdx i2s
                    reps' = foldl (\reps el -> IntMap.alter decrementRepetition (abs . fromIntegral . ilabel $ el) reps) reps l
                    decrementRepetition Nothing = error patternError
                    decrementRepetition (Just x)
                      | x == 1 = Nothing
                      | x > 1 = Just $ x - 1
                      | otherwise = error patternError
                in return . Just $ (n, (reps', ls1', ls2', (i1s' ++ bps1, i2s' ++ bps2)))

        -- Find longest substring of l1 and l2, when using withSin find the longest substring containing sin
        longestSubstring withSin sin l1 l2 = maxWith length
                                             . filter (\l -> not withSin || sin `elem` l || invOri sin `elem` l)
                                             . map (longestMatch $ T.construct l2)
                                             . filter (\l -> not withSin || sin `elem` l || invOri sin `elem` l)
                                             $ List.tails l1
          where longestMatch :: Eq el => T.STree el -> [el] -> [el]
                longestMatch T.Leaf _ = []
                longestMatch (T.Node edges) candidate =
                  maxWith length $ map (prefixMatch candidate) edges

                prefixMatch :: Eq el => [el] -> T.Edge el -> [el]
                prefixMatch candidate (p, tree)
                  | p' `List.isPrefixOf` candidate = p' ++ longestMatch tree (drop (length p') candidate)
                  | otherwise = commonPrefix p' candidate
                  where p' = T.prefix p
                commonPrefix (a:as) (b:bs)
                  | a==b = a:commonPrefix as bs
                  | otherwise = []
                commonPrefix _ _ = []

--------------------------------------- Other Functions --------------------------------

assigments :: I.AnyInstance -> I.AnyInstance
assigments (I.AnyInstance inst) = I.AnyInstance . subOptimalAssigment2 . optimalAssigment . subOptimalAssigment1 $ inst

useMapFromContracted :: forall inst inst' g g' a a' a'' mod. (I.ReaInstAll inst g a a'' mod, I.ReaInstGenome inst' g' a') => inst' -> inst -> BreakPoints -> inst
useMapFromContracted conInst inst bps = I.replaceGenomesSameType g1 g2 inst
  where
    cg1 = I.getSource conInst
    cg2 = I.getTarget conInst

    (_, l2, n, _, _) = I.unwrapInstanceAsLists inst
    cl1 = G.toListOfGenes cg1
    cl2 = G.toListOfGenes cg2

    g1 = G.fromListOfGenes . remMarks $ l1_mark
    g2 = G.fromListOfGenes . remMarks $ l2_mark
 
    remMarks = map remMark
    remMark (a,i) = G.toEndK i n a

    l2_mark = snd $ List.mapAccumL addMark2 IntMap.empty l2
    addMark2 map a = (map', (a, map' IntMap.! a'))
      where
        a' = abs . fromIntegral $ a
        map' = IntMap.alter f a' map
        f Nothing = Just 0
        f (Just x) = Just $ x + 1

    s2s_mark = map fromStrip $ break2Strips (snd bps) l2_mark
    s2s_map = IntMap.fromList . zipWith combineChrStrip cl2 $ s2s_mark
      where
        combineChrStrip a s =
          case getOri a of
            LR -> (fromIntegral a,s)
            RL -> (fromIntegral . invOri $ a, revStrip s)

    l1_mark = reverse . foldl addMark1 [] $ cl1
    addMark1 ls a = reverse l ++ ls
      where
        l = case getOri a of
              LR -> s2
              RL -> revStrip s2
        s2 = s2s_map IntMap.! (abs . fromIntegral $ a)

    revStrip = uncurry zip . ((fromStrip . invOri . toStrip @mod) *** reverse) . unzip

data ContractedData mod g a a' = ContData (g a) (g a') (IntMap.IntMap (Strip mod a)) deriving (Show)

contractMap :: forall inst g a a' mod m. (I.ReaInstAll inst g a a' mod, Gmap m) => inst -> m -> m -> (ContractedData mod g a a',m)
contractMap inst m2 m1 = (ContData g1 cg1' strip_map, recoverMap cpg1 cg1')
    where
        (g1,g2,n,_,_) = I.unwrapInstance inst
        l1 = G.toListOfGenes $ g1
        l2 = G.toListOfGenes $ g2
        pl1 = G.toListOfGenes . mapGenome' m1 $ g1
        pl2 = G.toListOfGenes . mapGenome' m2 $ g2
        bps@(bp1, bp2) = permutationBreaks @mod pl1 pl2

        (cl1,cl2) = contractedRepresentation' @mod l1 l2 n bps False
        cg1_ = (G.fromListOfGenes $ cl1 :: g a')
        cg2_ = G.fromListOfGenes $ cl2
        conInst = I.replaceGenomes cg1_ cg2_ inst
        cg1 = I.getSource conInst
        cg2 = I.getTarget conInst
        ri = I.getRepInfo conInst
        gen = makeGeneratorRep ri
        std = standardMap gen
        cpl2 = G.toListOfGenes . mapGenome' std $ cg2
        cpl1 = map stripToCGene . break2Strips bp1 $ pl1
        cpg1 = G.getPerm $ (G.fromListOfGenes $ cpl1 :: g a')
        cg1' = G.fromListOfGenes $ zipWith (\a b -> G.orientGene a b) cpl1 (G.toListOfGenes cg1)

        strip_map = IntMap.fromList . zipWith combineChrStrip cpl2 $ strips2
          where
            combineChrStrip :: a' -> Strip mod a -> (Int, Strip mod a)
            combineChrStrip a s =
              case getOri a of
                LR -> (fromIntegral a, s)
                RL -> (fromIntegral . invOri $ a, invOri s)

        stm = foldl addStrip stmEmpty . zipWith combineStripChr cpl2 $ strips2
          where
            combineStripChr :: a' -> Strip mod a -> (Int, Strip mod a)
            combineStripChr a s =
              case getOri s of
                LR -> (fromIntegral a, s)
                RL -> (fromIntegral . invOri $ a, invOri s)

            addStrip :: StripMap mod a a' -> (Int, Strip mod a) -> StripMap mod a a'
            addStrip stm (ca,s) = stmInsert s (fromIntegral ca) stm

        strips2 = break2Strips bp2 $ pl2

        stripToCGene s =
            case stmLookup s stm of
              Nothing -> error "Error on contractMap."
              Just a' -> combineOrientation (getOri s) a'

uncontractMap :: forall g a a' mod m. (I.FullGenome g a a', Model mod, Gmap m) => ContractedData mod g a a' -> m -> m
uncontractMap (ContData g1 cg1 strip_map) m = (recoverMap p1 g1)
  where
    cpl1 = G.toListOfGenes . mapGenome' m $ cg1
    p1 = G.getPerm $ (G.fromListOfGenes pl1 :: g a)

    pl1 = reverse . foldl expandStrip [] $ cpl1
    expandStrip ls a = reverse l ++ ls
      where
          l = fromStrip $ case getOri a of
                            LR -> s2
                            RL -> invOri s2
          s2 = strip_map IntMap.! (abs . fromIntegral $ a)

data ContractableGenerator gen inst = ContGen {currentInst :: inst, originalGen :: gen}
type instance SepGen (ContractableGenerator gen inst) m = ConstGen (ContractableGenerator gen inst) m

instance (I.ReaInstAll inst g a a' mod, GmapGenerator gen m, Gmap m) => GmapGenerator (ContractableGenerator gen inst) m where

    generateMaps = generateMaps . originalGen
    standardMap = standardMap . originalGen

    neighbors gen m = map (uncontractMap cd) $ neighbors (originalGen gen) cm
        where (cd,cm) = contractMap (currentInst gen) (standardMap gen) m

    combineMaps = combineMaps . originalGen
    randomCombination = randomCombination . originalGen
    flight = flight . originalGen
