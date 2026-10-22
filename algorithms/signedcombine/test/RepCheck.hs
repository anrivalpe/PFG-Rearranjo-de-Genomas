{-# LANGUAGE TemplateHaskell, DataKinds, FlexibleInstances #-}
module RepCheck (runTests, genMapOfGenome) where

--------------------------------------------------------
import Utils
import Types
import Mapping
import MappingRep
import Instance (RepStatus(..), getRepInfo)
import Perm (initPerm, finishPerm)
--------------------------------------------------------
import Properties (isIsomorfic)
import MapProperties (differentMappings, permMapIso, combineEquivalency)
import GenomeCheck (TestGenome(..), withGenome)
import InstanceCheck (RIStr)
import Aux (MRev)
--------------------------------------------------------
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)
--------------------------------------------------------
import Genome as G
import qualified Instance as I
--------------------------------------------------------
import qualified Data.Sequence as Seq
--------------------------------------------------------

instance Arbitrary MapOfRep where
  arbitrary = do
    TestGenome g <- arbitrary :: Gen (TestGenome IsStr SGl)
    genMapOfGenome g

genMapOfGenome :: (Genome g a) => g a -> Gen MapOfRep
genMapOfGenome g = do
    let occ = getOccList . occA $ g
    let ri = RepInfo (G.len g) (G.rep g) occ
    GmapPerm ri . Seq.fromList <$>
      mapM (\o -> choose (0,fac o - 1)) (take (fromIntegral . G.rep $ g) occ)

prop_differentMappings :: TestGenome IsStr SGl -> Property
prop_differentMappings (TestGenome g) =
  forAll (genMapOfGenome g) $ \m1 ->
  forAll (genMapOfGenome g) $ \m2 ->
    differentMappings m1 m2 g

prop_permMapIso :: TestGenome IsStr SGl -> Property
prop_permMapIso (TestGenome g) =
  forAll (genMapOfGenome g) $ \m ->
    permMapIso g m

prop_generateMapsCorrectSize :: Property
prop_generateMapsCorrectSize = withGenome IsStr generateMapsCorrectSize

prop_lehmerIso :: Int -> Integer -> Property
prop_lehmerIso o_ code_ =
  let o = Occ $ abs o_ `mod` 10 + 1
      code = code_ `mod` fac o in
    isIsomorfic (decodePerm o) (encodePerm o) code

prop_combineEquivalency :: RIStr MRev Gl SGene -> Property
prop_combineEquivalency inst =
  forAll (genMapOfGenome g1) $ \m1 ->
  forAll (genMapOfGenome g2) $ \m2 ->
  let m0 = standardMap (makeGeneratorRep $ getRepInfo inst) in
    combineEquivalency (makeGeneratorRep $ getRepInfo inst) g1 g2 m1 m2 m0
    where
      g1 = I.getSource inst
      g2 = I.getTarget inst

generateMapsCorrectSize :: (Genome g a) => g a -> Property
generateMapsCorrectSize g =
  let r = G.rep g in
  let ri = RepInfo (G.len g) (G.rep g) (getOccList . occA $ g) in
  forAll (choose (0, min (abs $ 2^r) 100)) $ \m ->
    monadicIO $ do
      pd <- run . runMon $ initPerm 1 Rev ""
      maps <- run . runMon $ generateMaps (makeGeneratorRep ri) m
      run $ finishPerm pd
      assert $ length maps == m

return []
runTests :: IO Bool
runTests = $quickCheckAll
