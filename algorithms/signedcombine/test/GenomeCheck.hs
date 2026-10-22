{-# LANGUAGE TemplateHaskell, Rank2Types, MultiWayIf, FlexibleContexts, FlexibleInstances, DerivingStrategies, GeneralisedNewtypeDeriving, KindSignatures, DataKinds, ScopedTypeVariables #-}

module GenomeCheck (runTests, TestGenome(..), withGenome, genReversion, genTransposition) where

-----------------------------------------------
import Genome
-----------------------------------------------
import Types
import Aux (RevOp(..), TransOp(..), rearrange)
import Instance (RepStatus(..))
-----------------------------------------------
import Properties (isIsomorfic)
-----------------------------------------------
import Test.QuickCheck
import Data.Coerce (coerce)
import Data.List (sort)
import Data.Foldable (toList)
-----------------------------------------------
import qualified Data.Set as Set
-----------------------------------------------

---------------- Generators --------------------------------------

newtype TestGenome (r :: RepStatus) ga = TestGenome {testGenome :: ga} deriving newtype (Show)

instance (Genome g a) => Arbitrary (TestGenome IsStr (g a)) where
  arbitrary = sized $ \n -> do
    l <- map (fromIntegral . abs) <$> (vectorOf n arbitrary :: Gen [Int])
    coins <- infiniteListOf arbitrary
    let swaps b v = if b then v else invOri v
    return . TestGenome . renumber . correctNumbersMulti . fromListOfGenes . zipWith swaps coins $ l
  shrink = convertToGenomes . shrinkIntList . convertToList
    where
      shrinkIntList :: [Int] -> [[Int]]
      shrinkIntList = shrinkList (const [])
      convertToList = map fromIntegral . toListOfGenes . testGenome
      convertToGenomes = map (TestGenome . correctNumbersMulti . fromListOfGenes . map fromIntegral)

instance (Genome g a) => Arbitrary (TestGenome IsDup (g a)) where
  arbitrary = sized $ \n -> do
    d <- abs <$> arbitrary
    TestGenome <$> genGenomeWithDup (Size n) (Dup d)
  shrink = coerce . shrink . (coerce :: TestGenome IsDup ga -> TestGenome IsStr ga)

instance (Genome g a) => Arbitrary (TestGenome IsPerm (g a)) where
  arbitrary = sized $ \n -> TestGenome <$> genGenomeWithDup (Size n) 0
  shrink = coerce . shrink . (coerce :: TestGenome IsPerm ga -> TestGenome IsStr ga)

genGenomeWithDup :: (Genome g a) => Size -> Dup -> Gen (g a)
genGenomeWithDup n d = do
  let list = map fromIntegral $ [1..d] ++ [1..d] ++ [(2*d + 1)..fromIntegral n]
  coins <- infiniteListOf arbitrary
  let swaps b v = if b then v else invOri v
  g <- shuffle list
  return . fromListOfGenes . zipWith swaps coins $ g

withGenome :: RepStatus -> (forall g a. (Genome g a) => g a -> Property) -> Property
withGenome rstatus f =
  forAll arbitrary $ \ori ->
  forAll arbitrary $ \isL -> case rstatus of
    IsStr -> if
      | ori && isL -> forAll (arbitrary :: Gen (TestGenome IsStr SGl)) (f . testGenome)
      | not ori && isL -> forAll (arbitrary :: Gen (TestGenome IsStr UGl)) (f . testGenome)
      | ori && not isL -> forAll (arbitrary :: Gen (TestGenome IsStr SGs)) (f . testGenome)
      | otherwise -> forAll (arbitrary :: Gen (TestGenome IsStr UGs)) (f . testGenome)
    IsDup -> if
      | ori && isL -> forAll (arbitrary :: Gen (TestGenome IsDup SGl)) (f . testGenome)
      | not ori && isL -> forAll (arbitrary :: Gen (TestGenome IsDup UGl)) (f . testGenome)
      | ori && not isL -> forAll (arbitrary :: Gen (TestGenome IsDup SGs)) (f . testGenome)
      | otherwise -> forAll (arbitrary :: Gen (TestGenome IsDup UGs)) (f . testGenome)
    IsPerm -> if
      | ori && isL -> forAll (arbitrary :: Gen (TestGenome IsPerm SGl)) (f . testGenome)
      | not ori && isL -> forAll (arbitrary :: Gen (TestGenome IsPerm UGl)) (f . testGenome)
      | ori && not isL -> forAll (arbitrary :: Gen (TestGenome IsPerm SGs)) (f . testGenome)
      | otherwise -> forAll (arbitrary :: Gen (TestGenome IsPerm UGs)) (f . testGenome)

genReversion :: Size -> Gen RevOp
genReversion size_ = do
  let size = fromIntegral size_
  i <- choose (1, size)
  j <- choose (i, size)
  return $ R (Idx i) (Idx j)

genTransposition :: Size -> Gen TransOp
genTransposition size_ = do
  let size = fromIntegral size_
  i <- choose (1, size)
  j <- choose (i+1, size)
  k <- choose (j+1, size+1)
  return $ T (Idx i) (Idx j) (Idx k)

---------------- Properties --------------------------------------

prop_orientGeneOrientsCorretly :: Int -> Int -> Bool -> Property
prop_orientGeneOrientsCorretly a b signal = property $
  (a /= 0 && b /= 0) ==>
  if signal
  then getOri a == getOri (orientGene a b)
  else getOri a' == getOri (orientGene a' b')
  where
    a' = fromIntegral a :: UGene
    b' = fromIntegral a :: UGene

prop_toAndFromString :: Property
prop_toAndFromString = withGenome IsStr (isIsomorfic toStringOfGenes  fromStringOfGenes)

prop_toAndFromBString :: Property
prop_toAndFromBString = withGenome IsStr (isIsomorfic toBStringOfGenes  fromBStringOfGenes)

prop_multipleReversions :: Property
prop_multipleReversions = withGenome IsStr multipleReversions

prop_transpositionWithReversions :: Property
prop_transpositionWithReversions = withGenome IsStr transpositionWithReversions

prop_mirrorMirrorPos :: Property
prop_mirrorMirrorPos = withGenome IsStr mirrorMirrorPos

prop_correctBeg :: Property
prop_correctBeg = withGenome IsStr correctBeg

prop_extendAndDel :: Property
prop_extendAndDel = withGenome IsStr extendAndDel

prop_correnspondenceBetweenGposAndGPos :: Property
prop_correnspondenceBetweenGposAndGPos = withGenome IsStr correnspondenceBetweenGposAndGPos

prop_correnspondenceBetweenGoccAndGOcc :: Property
prop_correnspondenceBetweenGoccAndGOcc = withGenome IsStr correnspondenceBetweenGoccAndGOcc

prop_compatibilityBetweenGPosAndGPosS :: Property
prop_compatibilityBetweenGPosAndGPosS = withGenome IsStr compatibilityBetweenGPosAndGPosS

---------------- Test Functions --------------------------------------

multipleReversions :: (Genome g a, Eq (g a)) => g a -> Property
multipleReversions g =
  forAll (listOf . genReversion $ len g) $ \revs ->
    g == (rearrange revs . rearrange (reverse revs) $ g)

transpositionWithReversions :: (Genome g a, Eq (g a)) => g a -> Property
transpositionWithReversions g =
  forAll (genTransposition $ len g) $ \(T i j k) ->
  trans i j k g == (rev i (k-1) . rev i (j-1) . rev j (k-1) $ g)

mirrorMirrorPos :: (Genome g a) => g a -> Property
mirrorMirrorPos g =
  let g' = mirror g in
  let n = len g in
  property $ (map (sort . pos g) . toList . alf $ g) == (map (sort . map (fromIntegral n+1-) . pos g') . toList . alf $ g')

correctBeg :: (Genome g a) => g a -> Property
correctBeg g = property $ len g == 0 || (minimum . pos g $ beg g) == 1

extendAndDel :: (Genome g a) => g a -> Property
extendAndDel g = property $ g == (delExt . extend $ g)

correnspondenceBetweenGposAndGPos :: (Genome g a) => g a -> Property
correnspondenceBetweenGposAndGPos g = property $ gposAns == gPosAns
  where
    gposAns = map (pos g) alf_l
    gPosAns = map (getPos poss) alf_l
    alf_l = toList . alf $ g
    poss = posA g

correnspondenceBetweenGoccAndGOcc :: (Genome g a) => g a -> Property
correnspondenceBetweenGoccAndGOcc g = property $ goccAns == gOccAns
  where
    goccAns = map (occ g) alf_l
    gOccAns = map (getOcc occs) alf_l
    alf_l = toList . alf $ g
    occs = occA g

compatibilityBetweenGPosAndGPosS :: (Genome g a) => g a -> Property
compatibilityBetweenGPosAndGPosS g = property $ gPosAns == gPosSAns
  where
    gPosAns = map (\a -> Set.union (getPosSetOri posSs a) (getPosSetOri posSs (invOri a))) alf_l
    gPosSAns = map (getPosSet poss) alf_l
    alf_l = toList . alf $ g
    getPosSet poss_ = Set.fromList . getPos poss_
    getPosSetOri poss_ = Set.fromList . getPosOri poss_
    poss = posA g
    posSs = posAS g

return []
runTests :: IO Bool
runTests = $quickCheckAll
