{-# LANGUAGE TypeApplications, ScopedTypeVariables, FlexibleContexts, GADTs, TupleSections, MultiParamTypeClasses, FlexibleInstances #-}

module SoarAux
  ( PMGraph3
  , PMGraph4
  , PMGraph(..)
  , getBPFromIS
  , subOptimalAssigment1
  , subOptimalAssigment2
  , optimalAssigment
  , applyAssigmentInLoop
  , contractedRepresentation
  , cicleDecomposition
  , makePMGraph
  ) where

--------------------------------------------------
import Utils
import Types
import Aux
--------------------------------------------------
import Control.Arrow (second, (***))
import Control.Monad.State.Lazy (State, runState, get, put)
import Data.Array (listArray, Array, (!), array, accumArray)
import Data.Bits (testBit, setBit, zeroBits, complement, (.|.), Bits)
import Data.Maybe (mapMaybe, listToMaybe, isNothing)
import Data.Proxy (Proxy(..))
import Data.Graph (Graph, Edge, Vertex, buildG)
import Data.Graph.Inductive.Graph (mkGraph)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.DFS (xdfsWith)
import Data.Graph.Inductive.Query.MaxFlow (mf)
import Data.Foldable (toList)
import Data.IntMap (IntMap)
import Data.Sequence (Seq(..), (><))
import Data.Set (Set)
import Data.Tuple (swap)
--------------------------------------------------
import qualified Instance as I
import qualified Genome as G
--------------------------------------------------
import qualified Data.Sequence as Seq
import qualified Data.List as List
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set
--------------------------------------------------

data PMGraph3 mod a = PMGraph3 [Edge] Size (PairMaches mod a) deriving (Show)
data PMGraph4 mod a = PMGraph4 [Edge] Size (PairMaches mod a) deriving (Show)
type MapGraph = IntMap [Vertex]

class PMGraph p mod where
  makePMGraph_ :: [Edge] -> Size -> PairMaches mod a -> p mod a
  independentSet :: p mod a -> BitMask
  vertexCover :: p mod a -> BitMask
  getPM :: p mod a -> PairMaches mod a
  numVertex :: p mod a -> Int

  independentSet = complement . vertexCover
  vertexCover = complement . independentSet

makePMGraph :: (I.ReaInstGenMod inst g a mod, PMGraph p mod) => Proxy p -> inst -> p mod a
makePMGraph _ inst = makePMGraph_ edges n pms
  where
    edges = findEgdes pms rpms
    pms = getPairMachesFromInstance inst
    rpms = getPairMachesFromInstanceRev inst
    n = pmsLength pms

contractedRepresentation :: forall inst g a a' mod. (I.ReaInstAll inst g a a' mod) => BreakPoints -> Bool -> inst -> I.AnyInstance
contractedRepresentation bps asPerm inst = I.AnyInstance $ I.replaceGenomes g1' g2' inst
      where
        (l1',l2') = contractedRepresentation' @mod l1 l2 n bps asPerm
        g1' = (G.fromListOfGenes l1') :: g a'
        g2' = (G.fromListOfGenes l2') :: g a'
        (l1, l2, n, _, _) = I.unwrapInstanceAsLists inst

instance PMGraph PMGraph4 mod where
  makePMGraph_ = PMGraph4

  vertexCover (PMGraph4 edges _ _) = foldl coverEdges zeroBits edges
    where
      coverEdges :: BitMask -> Edge -> BitMask
      coverEdges onCover (u,v)
        | not (testBit onCover u) && not (testBit onCover v) = setBit (setBit onCover u) v
        | otherwise = onCover

  numVertex (PMGraph4 _ n _) = fromIntegral n

  getPM (PMGraph4 _ _ pms) = pms

instance PMGraph PMGraph3 mod where
  makePMGraph_ = PMGraph3

  independentSet (PMGraph3 edges n _) = preIndSet .|. sheIndSet
    where
      (preEdges, preIndSet) = preproces edges (fromIntegral n)
      graph = IntMap.fromListWith (++) $ second (:[]) <$> preEdges
      sheIndSet = shearerIndSet . removeTriangles $ graph

  numVertex (PMGraph3 _ n _) = fromIntegral n

  getPM (PMGraph3 _ _ pms) = pms

findEgdes :: (G.Gene a, Model mod) => PairMaches mod a -> PairMaches mod a -> [Edge]
findEgdes pms rpms = foldMap findEdge pms
  where
    findEdge pm@((Due a1 a2),_,_,_,vtx) =
      mapMaybe (\pm'@(_,_,_,_,vtx') ->
                  if isConflict pm pm'
                  then Just (vtx,vtx')
                  else Nothing) l
      where
        l = uniqueSort $ pmsList pms a1 ++ pmsList pms (invOri a1) ++ pmsList pms a2 ++ pmsList pms (invOri a2) ++  pmsList rpms a1 ++  pmsList rpms (invOri a1) ++  pmsList rpms a2 ++  pmsList rpms (invOri a2)

preproces :: [Edge] -> Int -> ([Edge], BitMask)
preproces edges n = (edges', indSet)
  where
    edges' = filter (\(u,v) -> testBit half u && testBit half v) edges

    indSet :: BitMask
    indSet = foldl setBit zeroBits indSetNodes
    indSetNodes = filter inIndSet nodes
    inIndSet v = (v `elem` minCut) && ((v + n) `notElem` minCut)

    half :: BitMask
    half = foldl setBit zeroBits halfNodes
    halfNodes = filter inHalf nodes
    inHalf v = (v `elem` minCut) && ((v + n) `elem` minCut) ||
               (v `notElem` minCut) && ((v + n) `notElem` minCut)

    bipGraph :: Gr () Int
    bipGraph = mkGraph lNodes lEdges
    mfG = mf bipGraph s t
    minCut = xdfsWith next (\(_,v,_,_) -> v) [s] mfG
    next (_, _, _, origem) = mapMaybe (\((_,_,residual), v) ->
                                       if residual > 0 then Just v else Nothing) origem
    n' = 2*n + 2
    s = 0
    t = n' - 1
    nodes = [1..n]
    lNodes = zip [s..t] (repeat ())
    lEdges = map (\v -> (v + n,t,1)) nodes ++ map (s,,1) nodes ++ foldMap newEdges edges
    newEdges (v1,v2) = [(v1,v2 + n,n'),(v2,v1 + n,n')]

removeTriangles :: MapGraph -> MapGraph
removeTriangles graph = IntMap.fromList . mapMaybe delTriangles . IntMap.assocs $ graph
  where
    inTriangle :: BitMask
    inTriangle = foldl checkNeigh zeroBits $ IntMap.keys graph
    checkNeigh inTriangle' vtx =
      if testBit inTriangle' vtx
      then inTriangle'
      else  addToTriangle . listToMaybe . filter hasEdge $ allNeigPairs
      where
        addToTriangle Nothing = inTriangle'
        addToTriangle (Just (u,v)) = setBits inTriangle' [vtx,u,v]
        neig = graph IntMap.! vtx
        allNeigPairs = (,) <$> neig <*> neig
        hasEdge (u,v) = not (testBit inTriangle' u) && not (testBit inTriangle' v) && v `elem` (graph IntMap.! u)

    delTriangles (vtx, adj) =
      if testBit inTriangle vtx
      then Nothing
      else Just (vtx, filter (not . testBit inTriangle) adj)

shearerIndSet :: MapGraph -> BitMask
shearerIndSet graph = removeVertice graph zeroBits
  where
    removeVertice :: MapGraph -> BitMask -> BitMask
    removeVertice g indSet
      | n == 0 = indSet
      | d == 0 = setBits indSet vtxs
      | fromIntegral n <= d / f d = setBits indSet lasts
      | otherwise = removeVertice g' indSet'
      where
        chosen = case filter test vtxs of
                   (x:_) -> x
                   [] -> error patternError
        indSet' = setBit indSet chosen
        toRemove = chosen:(g IntMap.! chosen)
        g' = (List.\\ toRemove) <$> foldl (flip IntMap.delete) g toRemove
        n = IntMap.size g
        outDeg = fmap length g
        vtxs = IntMap.keys g
        lasts = (g IntMap.! ) . head . filter ((>= d) . fromIntegral . (outDeg IntMap.! )) $ vtxs

        d = avg outDeg
        f 0 = 0
        f 1 = 0.5
        f x = (x * log x - x + 1.0) / (x - 1.0)**2
        f' 0 = 0
        f' 1 = 0
        f' x = - ((x + 1.0) * log x - 2*x + 2) / (x - 1.0)**3

        test p = (d1 + 1.0) * f d <= 1.000000001 + (d*d1 + d - 2.0*d1*d2) * f' d
          where
            d1 = fromIntegral $ outDeg IntMap.! p
            d2 = avg . map (outDeg IntMap.!) $ (g IntMap.! p)

type Queue = Seq (Vertex, IntMap Vertex, BitMask, Int)

cicleDecomposition :: forall inst g a. (I.ReaInstGenome inst g a) => inst -> inst
cicleDecomposition inst =
  if I.isPerm inst
  then inst
  else assigmentsOnGenomes changes inst
  where
    changes = unzip . concat $ ans
    (ans,_) = runState removeAllCicles (IntMap.empty, zeroBits, 1, [])
    (l1_, l2_, n_, _, _) = I.unwrapInstanceAsLists inst
    l1 = map fromIntegral l1_
    l2 = map fromIntegral l2_
    n = fromIntegral n_

    removeAllCicles = sequence $ removeCicle <$> verts

    verts = [(-2*n)..(-1)] ++ [1..2*n]
    vertsBouds = (-2*n,2*n+4)
    dummes = [2*n + 1, 2*n + 2, 2*n + 3, 2*n + 4]
    blacks :: Array Vertex Vertex
    blacks = array vertsBouds $ toBlack vtxs1 ++ toBlack vtxs2
      ++ bExts ++ map swap bExts
      where bExts = [ ((oris1 ! 1) * (-1), 2*n + 1),
                      ((oris1 ! n) * n, 2*n + 2),
                      ((oris2 ! 1) * (-n-1), 2*n + 3),
                      ((oris2 ! n) * (n+n), 2*n + 4),
                      (0,0)
                    ]
    grays :: Graph
    grays = buildG vertsBouds $ gEdges ++ map swap gEdges
      where gEdges = concat (toGray <$> zip l1 vtxs1)
              ++ [(2*n + 1, 2*n + 3), (2*n + 2, 2*n + 4)]

    toBlack :: [(Vertex,Vertex)] -> [(Vertex,Vertex)]
    toBlack vtxs = bEdges ++ map swap bEdges
      where
        bEdges = (\((_,x),(y,_)) -> (x,y)) <$> lPairs vtxs
    toGray :: (Int, (Vertex,Vertex)) -> [(Vertex,Vertex)]
    toGray (a,(v1,v2)) = concatMap pairVtxs vtxs
      where
        vtxs = gToV ! abs a
        pairVtxs (v1',v2') = [(mi,mi'),(ma,ma')]
          where (mi,ma,mi',ma') =
                  (min v1 v2, max v1 v2, min v1' v2', max v1' v2')

    vtxs1 :: [(Vertex,Vertex)]
    vtxs1 = zipWith (curry toVtxs) [1..n] l1
    vtxs2 :: [(Vertex,Vertex)]
    vtxs2 = zipWith (curry toVtxs) [(n+1)..(2*n)] l2
    toVtxs (i,a) = if a < 0 then (i,-i) else (-i,i)
    gToV :: Array Int [(Vertex,Vertex)]
    gToV = accumArray (++) [] (1,n) $ (abs *** (:[])) <$> zip l2 vtxs2

    alter = negate
    oris1 = listArray (1,n) $ map sign l1
    oris2 = listArray (1,n) $ map sign l2
    markVertices :: BitMask -> [Vertex] -> BitMask
    markVertices m vs = setBits m $ (\v -> v+2*n) <$> vs
    testVertice :: BitMask -> Vertex -> Bool
    testVertice m v = testBit m (v+2*n)

    removeCicle :: Vertex -> State (IntMap Vertex, BitMask, Int, [[Int]]) [(Ass a, Ass a)]
    removeCicle root = do
      (fixed,done,count,cycles) <- get
      if testVertice done root
      then return []
      else do
        let fixed' = bfs True (Seq.singleton (root,fixed,done,0)) (Set.singleton (root,0))
            (count', done', assigments, cycle) = assWithCycle root fixed' done [] count []
        put (fixed',done',count',cycle:cycles)
        return assigments
      where
        assWithCycle :: Vertex -> IntMap Vertex -> BitMask -> [(Ass a, Ass a)] -> Int -> [Int] -> (Int, BitMask, [(Ass a,Ass a)], [Int])
        assWithCycle v f done acc a cycle
          | v == root && testVertice done root = (a,done',acc,cycle)
          | v `elem` dummes = assWithCycle next f done' acc a (v:cycle)
          | otherwise = assWithCycle next f done' (ass:acc) (a+1) (v:cycle)
          where
            par = f IntMap.! v
            done' = markVertices done [v,par]
            next = blacks ! par
            ass = let (i1,i2) =
                        if abs v <= n
                        then (abs v, abs par - n)
                        else (abs par, abs v - n)
                  in ((Idx i1, fromIntegral $ (oris1 ! i1) * a), (Idx i2, fromIntegral $ (oris2 ! i2) * a))

        bfs :: Bool -> Queue -> Set (Vertex,Int) -> IntMap Vertex
        bfs _ Seq.Empty _ = error patternError
        bfs isFirst ((v_,fixed,vizited,level) :<| qs) vizited_in_level =
          if v_ == root && not isFirst
          then fixed
          else bfs False qs' vizited_in_level'
          where
            v = blacks ! v_
            neigs = maybe notFixed (:[]) (IntMap.lookup v fixed)
            notFixed = filter (`IntMap.notMember` fixed) $ grays ! v
            notDone = filter (\x -> not (Set.member (x,level+1) vizited_in_level || testVertice vizited x)) neigs
            qs' = qs >< Seq.fromList new
            new = map (\x -> (x, fixEdge x, markVizited x, level+1)) notDone
            vizited_in_level' = foldl (\s x -> Set.insert (x,level + 1) s) vizited_in_level notDone
            markVizited u = markVertices vizited [u]
            fixEdge u = IntMap.insert (alter v) (alter u) . IntMap.insert (alter u) (alter v) . IntMap.insert v u . IntMap.insert u v $ fixed

getBPFromIS :: forall inst g a mod. (I.ReaInstGenMod inst g a mod) => inst -> PairMaches mod a -> BitMask -> BreakPoints
getBPFromIS inst pms indSet = (partition False l1, partition True l2)
  where
    (l1, l2, n, _, _) = I.unwrapInstanceAsLists inst
    partition isSecond = map fst . filter isBreakpoint . zip [1..fromIntegral n] . map (toDue @mod). lPairs
      where
        isBreakpoint (i,due) = isNothing . List.find (\(_,_,idx1,idx2,v) ->
                            (if isSecond then idx2 == i else idx1 == i)
                            && testBit indSet v) $ pmsFind pms due

setBits :: (Bits b) => b -> [Int] -> b
setBits = foldl setBit

-------------------------- Assigments ------------------------------

applyAssigmentInLoop :: (I.ReaInstGenome inst g a) => (inst -> inst) -> inst -> inst
-- applyAssigmentInLoop ass inst = let (n,inst') = go 0 inst in seq (traceValue n) inst'
applyAssigmentInLoop ass = snd . go 0
  where
    go n inst = let inst' = ass inst in
               if inst' == inst
                  then (n,inst')
                  else go (n+1) inst'

optimalAssigment :: forall g a inst mod. (I.ReaInstGenMod inst g a mod) => inst -> inst
optimalAssigment inst = I.applyAssOnInstance (changes1, changes2) inst
  where
    n = I.len inst
    d = I.dup inst
    r = I.rep inst

    pms = getPairMachesFromInstance inst
    rpms = getPairMachesFromInstanceRev inst
    changes1 = recordChanges emptyAssigmets . fst $ changes
    changes2 = recordChanges emptyAssigmets . snd $ changes
    changes :: (G.Gene a) => ([Ass a],[Ass a])
    changes = unzip . foldMap findChanges $ pms

    findChanges :: (G.Gene a) => PairMacheEntry mod a -> [(Ass a,Ass a)]
    findChanges pm@(Due a1 a2, Due a1' a2', idx1, idx2, _)
      | I.duplicated d a1 && I.singleton r a2 =
        maybe [] combineChanges1 $ secondPM a1 idx1 pm
      | I.singleton r a1 && I.duplicated d a2 =
        maybe [] combineChanges2 $ secondPM a2 (idx1+1) pm
      | otherwise = []
        where
          new1 = G.toEnd n a1
          combineChanges1 x = [x, ((idx1, new1),
                                  if a1 == a1' then (idx2,new1) else (idx2+1, invOri new1) )]
          new2 = G.toEnd n a2
          combineChanges2 x = [x, ((idx1+1, new2),
                                  if a2 == a2' then (idx2+1,new2) else (idx2, invOri new2) )]

    secondPM :: a -> Idx -> PairMacheEntry mod a -> Maybe (Ass a, Ass a)
    secondPM dup_a idx pm = listToMaybe . mapMaybe getChange2 $ l
      where
        l = pmsList pms dup_a ++ pmsList pms (invOri dup_a) ++ pmsList rpms dup_a ++ pmsList rpms (invOri dup_a)
        getChange2 pm'@(Due a1 a2, Due a1' a2', idx1, idx2, _)
          | isConflict pm pm' = Nothing
          | idx1 > idx && abs a1 == abs dup_a && I.singleton r a2 =
            Just change1
          | idx1+1 > idx && I.singleton r a1 && abs a2 == abs dup_a =
            Just change2
          | otherwise = Nothing
          where
            new1 = G.toEnd n . G.toEnd n $ a1
            change1 = ((idx1, new1), if a1 == a1'
                                     then (idx2, new1)
                                     else (idx2+1, invOri new1))
            new2 = G.toEnd n . G.toEnd n $ a2
            change2 = ((idx1+1, new2), if a2 == a2'
                                       then (idx2+1, new2)
                                       else (idx2, invOri new2))

subOptimalAssigment1 :: forall g a inst mod. (I.ReaInstGenMod inst g a mod) => inst -> inst
subOptimalAssigment1 inst = assigmentsOnGenomes changes inst
  where
    (l1, l2, n, _, r) = I.unwrapInstanceAsLists inst

    changes :: ([Ass a],[Ass a])
    changes = unzip . concat . snd . List.mapAccumL findChanges (zeroBits,zeroBits,fromIntegral n) $ candidates

    triples1 = lTriples l1
    triples2 = accumArray (++) [] (1, fromIntegral n) . zipWith (curry toArray) [2..] $ lTriples l2
    toArray e@(_, (_,k,_)) = (abs k, [e])
    validTriple (a1,a2,a3) = I.singleton r a1 && I.replicated r a2 && I.singleton r a3
    invTriple tri@(a1,a2,a3) = case model @mod of
      Trans -> tri
      Rev -> (invOri a3, invOri a2, invOri a1)
      TransRev -> (invOri a3, invOri a2, invOri a1)

    candidates = filter (validTriple . snd) $ zip [2..] triples1

    findChanges :: (BitMask,BitMask,a) -> (Idx, (a,a,a)) -> ((BitMask,BitMask,a), [(Ass a, Ass a)])
    findChanges acc (idx1, tri1@(_,dup_a,_)) = second concat . List.mapAccumL findMatch acc $ triples2 ! abs dup_a
        where
          findMatch :: (BitMask,BitMask,a) -> (Idx, (a,a,a)) -> ((BitMask,BitMask,a), [(Ass a, Ass a)])
          findMatch acc' (idx2, tri2@(_,dup_a',_)) =
            if tri1 == tri2 || tri1 == invTriple tri2
            then setChange dup_a dup_a' idx1 idx2 acc'
            else (acc', [])

subOptimalAssigment2 :: forall g a inst mod. (I.ReaInstGenMod inst g a mod) => inst -> inst
subOptimalAssigment2 inst = assigmentsOnGenomes changes inst
  where
    n = I.len inst
    r = I.rep inst

    changes :: ([Ass a],[Ass a])
    changes = unzip . concat . snd . List.mapAccumL findChange (zeroBits,zeroBits,fromIntegral n) . toList $ pms

    pms = getPairMachesFromInstance inst

    findChange :: (BitMask,BitMask,a) -> PairMacheEntry mod a -> ((BitMask,BitMask,a), [(Ass a,Ass a)])
    findChange acc (Due a1 a2, Due a1' a2', idx1_, idx2_, _)
      | I.replicated r a1 && I.singleton r a2 =
          setChange a1 a1' idx1_ (if a1 == a1' then idx2_ else idx2_+1) acc
      | I.singleton r a1 && I.replicated r a2 =
          setChange a2 a2' (idx1_+1) (if a2 == a2' then idx2_+1 else idx2_) acc
      | otherwise = (acc, [])

setChange :: (G.Gene a) => a -> a -> Idx -> Idx -> (BitMask,BitMask,a) -> ((BitMask, BitMask, a), [(Ass a, Ass a)])
setChange dup_a dup_a' idx1 idx2 acc@(mapped1, mapped2, new) =
  if testBit mapped1 (fromIntegral idx1) || testBit mapped2(fromIntegral idx2)
  then (acc, [])
  else ((setBit mapped1 (fromIntegral idx1), setBit mapped2 (fromIntegral idx2), new + 1), [((idx1,new1),(idx2,new2))])
  where
    new1 = G.orientGene dup_a $ new + 1
    new2 = if dup_a == dup_a' then new1 else invOri new1

getPairMachesFromInstance :: (I.ReaInstGenMod inst g a mod) => inst -> PairMaches mod a
getPairMachesFromInstance inst = getPairMaches LR l1 l2 n
  where
    (l1, l2, n, _, _) = I.unwrapInstanceAsLists inst

getPairMachesFromInstanceRev :: (I.ReaInstGenMod inst g a mod) => inst -> PairMaches mod a
getPairMachesFromInstanceRev inst = getPairMaches RL l1 l2 n
  where
    (l1, l2, n, _, _) = I.unwrapInstanceAsLists inst

assigmentsOnGenomes :: forall g a inst . (I.ReaInstGenome inst g a) => ([Ass a],[Ass a]) -> inst -> inst
assigmentsOnGenomes changes = I.applyAssOnInstance (ass1,ass2)
  where (ass1,ass2) = both (recordChanges emptyAssigmets) changes
