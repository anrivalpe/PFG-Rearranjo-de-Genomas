{-# LANGUAGE TemplateHaskell, Rank2Types, MultiWayIf, TypeFamilies, ScopedTypeVariables, DataKinds, FlexibleInstances, TupleSections, TypeApplications, FlexibleContexts #-}

module InstanceCheck (runTests, withRearrangeInstance, withAnyRearrangeInstance, withRearrangeInstance', RIDup, RIStr) where

-----------------------------------------------------------------------
import Types
import Aux
import GenomeCheck (withGenome, genReversion, genTransposition, TestGenome(..))
import Extremities (cutEdges)
import SoarAux (subOptimalAssigment1, subOptimalAssigment2, optimalAssigment)
-----------------------------------------------------------------------------
import Data.Proxy (Proxy(..))
-----------------------------------------------------------------------------
import Test.QuickCheck
-----------------------------------------------------------------------------
import Genome as G
import Instance as I
-----------------------------------------------------------------------------
import qualified Data.IntMap as IntMap
-----------------------------------------------------------------------------

---------------- Generators --------------------------------------

instance (Genome g a, Model mod) => Arbitrary (RIStr mod g a) where
  arbitrary = do
    g1 <- testGenome <$> (arbitrary :: Gen (TestGenome IsStr (g a)))
    g2 <- (fromListOfGenes <$> shuffle (toListOfGenes g1)) :: Gen (g a)
    return $ buildInstance @IsStr g1 g2

  shrink inst = map (\(g1',g2') -> replaceGenomesSameType g1' g2' inst) gs
    where
      g1 = getSource inst
      g2 = getTarget inst
      g1s = testGenome <$> shrink (TestGenome g1 :: TestGenome IsStr (g a))
      gs = [(g1',shrinkedTarget g1' g2) | g1' <- g1s]

instance (Genome g a, Model mod) => Arbitrary (RIDup mod g a) where
  arbitrary = do
    g1 <- testGenome <$> (arbitrary :: Gen (TestGenome IsDup (g a)))
    g2 <- (fromListOfGenes <$> shuffle (toListOfGenes g1)) :: Gen (g a)
    return $ buildInstance @IsDup g1 g2

  shrink inst = map (\(g1',g2') -> replaceGenomesSameType g1' g2' inst) gs
    where
      g1 = getSource inst
      g2 = getTarget inst
      g1s = testGenome <$> shrink (TestGenome g1 :: TestGenome IsDup (g a))
      gs = [(g1',shrinkedTarget g1' g2) | g1' <- g1s]

instance (Genome g a, Model mod) => Arbitrary (RIPerm mod g a) where
  arbitrary = do
    g1 <- testGenome <$> (arbitrary :: Gen (TestGenome IsPerm (g a)))
    g2 <- (fromListOfGenes <$> shuffle (toListOfGenes g1)) :: Gen (g a)
    return $ buildInstance @IsPerm g1 g2

  shrink inst = map (\(g1',g2') -> replaceGenomesSameType g1' g2' inst) gs
    where
      g1 = getSource inst
      g2 = getTarget inst
      g1s = testGenome <$> shrink (TestGenome g1 :: TestGenome IsPerm (g a))
      gs = [(g1',shrinkedTarget g1' g2) | g1' <- g1s]

shrinkedTarget :: (Genome g a) => g a -> g a -> g a
shrinkedTarget g1' = asListOfGenes (f [] . (e,))
  where
    e = IntMap.fromListWith (+) . flip zip (repeat 1) $ fromIntegral <$> toListOfGenes g1'
    f acc (_,[]) = acc
    f acc (m,l:ls) = let il = fromIntegral l in
      if fromIntegral il `IntMap.member` m
         then f (l:acc) (IntMap.update up il m, ls)
         else f acc (m,ls)
    up 1 = Nothing
    up v = Just $ v - 1

withAnyRearrangeInstance :: RepStatus -> (I.AnyInstance -> Property) -> Property
withAnyRearrangeInstance = withRearrangeInstance_ False

withRearrangeInstance :: RepStatus -> (forall g a a' inst mod. (ReaInstAll inst g a a' mod, Model mod) => inst -> Property) -> Property
withRearrangeInstance = withRearrangeInstance' False

withRearrangeInstance' :: Bool -> RepStatus -> (forall g a a' inst mod. (ReaInstAll inst g a a' mod, Model mod) => inst -> Property) -> Property
withRearrangeInstance' onlyRev rstatus f = withRearrangeInstance_ onlyRev rstatus anyf
   where
       anyf :: AnyInstance -> Property
       anyf (AnyInstance inst) = f inst

withRearrangeInstance_ :: Bool -> RepStatus -> (I.AnyInstance -> Property) -> Property
withRearrangeInstance_ onlyRev rstatus f =
    forAll arbitrary $ \ori ->
    forAll arbitrary $ \isL ->
    forAll (elements (if
                         | onlyRev -> [Rev]
                         | ori -> [Rev,TransRev]
                         | otherwise -> [Rev,Trans,TransRev])) $ \mod ->
    let f' = f . readCorrectInstance rstatus (if isL then GList else GSeq) (if ori then Signed else Unsigned) mod . writeInstance in
    if ori
       then (case rstatus of
               IsPerm -> forAll (arbitrary :: Gen (RIPerm MRev Gl SGene)) (f' . AnyInstance)
               IsDup -> forAll (arbitrary :: Gen (RIDup MRev Gl SGene)) (f' . AnyInstance)
               IsStr -> forAll (arbitrary :: Gen (RIStr MRev Gl SGene)) (f' . AnyInstance))
       else (case rstatus of
               IsPerm -> forAll (arbitrary :: Gen (RIPerm MRev Gl UGene)) (f' . AnyInstance)
               IsDup -> forAll (arbitrary :: Gen (RIDup MRev Gl UGene)) (f' . AnyInstance)
               IsStr -> forAll (arbitrary :: Gen (RIStr MRev Gl UGene)) (f' . AnyInstance))

withGenomeAndModel :: RepStatus -> (forall g a mod. (Genome g a, Model mod) => Proxy mod -> g a -> Property) -> Property
withGenomeAndModel rstatus f =
    forAll (elements [Rev,Trans,TransRev]) $
        \mod -> case mod of
                  Rev -> withGenome rstatus $ f (Proxy @MRev) 
                  Trans -> withGenome rstatus $ f (Proxy @MTrans) 
                  TransRev -> withGenome rstatus $ f (Proxy @MTransRev) 

---------------- Properties --------------------------------------

prop_sameAreBalanced :: Property
prop_sameAreBalanced = withGenomeAndModel IsStr $ sameAreBalanced

prop_operationsMantainBalance :: Property
prop_operationsMantainBalance = withGenomeAndModel IsStr $ operationsMantainBalance

prop_cutEdgesEmptyTheInstance :: Property
prop_cutEdgesEmptyTheInstance = withGenomeAndModel IsStr $ cutEdgesEmptyTheInstance

prop_cutEdgesMantainBalance :: Bool -> Property
prop_cutEdgesMantainBalance dup = withRearrangeInstance (if dup then IsDup else IsStr) cutEdgesMantainBalance

prop_assigmentsMantainBalance :: Bool -> Property
prop_assigmentsMantainBalance dup = withRearrangeInstance (if dup then IsDup else IsStr) assigmentsMantainBalance

---------------- Test Functions --------------------------------------

sameAreBalanced :: forall mod g a. (Genome g a, Model mod) => Proxy mod -> g a -> Property
sameAreBalanced _ g = property . balanced $ (buildInstance @IsStr g g :: I.RIStr mod g a)

cutEdgesEmptyTheInstance :: forall mod g a. (Genome g a, Model mod) => Proxy mod -> g a -> Property
cutEdgesEmptyTheInstance _ g = property . I.empty . cutEdges $ inst
  where inst = (I.buildInstance @IsStr g g :: I.RIStr mod g a)

cutEdgesMantainBalance :: (I.ReaInstGenome inst g a) => inst -> Property
cutEdgesMantainBalance = property . I.balanced . cutEdges

operationsMantainBalance :: forall mod g a. (Genome g a, Model mod) => Proxy mod -> g a -> Property
operationsMantainBalance _ g1 =
  forAll (listOf . genReversion $ G.len g1) $ \revs ->
  forAll (listOf . genTransposition $ G.len g1) $ \transs ->
  let g2 = rearrange revs . rearrange transs $ g1 in
  balanced (I.buildInstance @IsStr g1 g2 :: I.RIStr mod g a)

assigmentsMantainBalance :: (ReaInstAll inst g a a' mod) => inst -> Property
assigmentsMantainBalance inst = property $
  I.balanced . subOptimalAssigment2 . subOptimalAssigment1 . optimalAssigment $ inst

return []
runTests :: IO Bool
runTests = $quickCheckAll
