{-# LANGUAGE TemplateHaskell, DataKinds, FlexibleInstances #-}
module DupCheck (runTests) where

--------------------------------------------------------
import Utils
import Types
import Args (MutMethod(..), CrossMethod(..))
import Mapping
import TabuSearch
import MappingDup
import Genome (Genome, SGl, dup)
import Instance (RepStatus(..))
import Perm (initPerm, finishPerm)
--------------------------------------------------------
import MapProperties (differentMappings)
import GenomeCheck (TestGenome(..), withGenome)
--------------------------------------------------------
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Data.Bits (bit)
import Data.Coerce (coerce)
import Data.List (intersect)
import Control.Monad.Random (evalRandIO)
import Control.Monad (replicateM)
--------------------------------------------------------

---------------- Generators --------------------------------------

instance Arbitrary MapOfDup where
  arbitrary = sized $ \d_ -> do
      let d = Dup d_
      m <- choose (bit 0, bit (fromIntegral d))
      return $ makeMapOfDup d m

instance Arbitrary MoveDup where
  arbitrary = getMove <$> arbitrary <*> arbitrary

---------------- Properties --------------------------------------

prop_differentMappings :: TestGenome IsDup SGl -> Property
prop_differentMappings (TestGenome g) =
  let d = fromIntegral $ dup g in
  forAll (resize d arbitrary :: Gen MapOfDup) $ \m1 ->
  forAll (resize d arbitrary :: Gen MapOfDup) $ \m2 ->
    differentMappings m1 m2 g

prop_restrictedMovesAreNotGenerated :: Positive Int -> Positive Int -> Property
prop_restrictedMovesAreNotGenerated d_ num_ =
  forAll (replicateM num $ resize d arbitrary :: Gen [MoveDup]) $ \movs ->
  forAll (resize d arbitrary :: Gen MapOfDup) $ \m ->
    restrictedMovesAreNotGenerated movs m
      where d = coerce d_; num = coerce num_

prop_mutationMRCreateNew :: Positive Int -> Property
prop_mutationMRCreateNew d_ = forAll (resize d arbitrary :: Gen MapOfDup) $ mutationCreateNew (Dup d) MR
  where d = coerce d_

prop_mutationMBCreateNew :: Positive Int -> Property
prop_mutationMBCreateNew d_ = forAll (resize d arbitrary :: Gen MapOfDup) $ mutationCreateNew (Dup d) MB
  where d = coerce d_

prop_crossoverXRWithSameDoesNotChange :: Positive Int -> Property
prop_crossoverXRWithSameDoesNotChange d_ = forAll (resize d arbitrary :: Gen MapOfDup) $ crossoverWithSameDoesNotChange (Dup d) XR
  where d = coerce d_

prop_crossoverXBWithSameDoesNotChange :: Positive Int -> Property
prop_crossoverXBWithSameDoesNotChange d_ = forAll (resize d arbitrary :: Gen MapOfDup) $ crossoverWithSameDoesNotChange (Dup d) XB
  where d = coerce d_

prop_crossoverXPWithSameDoesNotChange :: Positive Int -> Property
prop_crossoverXPWithSameDoesNotChange d_ = forAll (resize d arbitrary :: Gen MapOfDup) $ crossoverWithSameDoesNotChange (Dup d) XP
  where d = coerce d_

prop_crossoverXMWithSameDoesNotChange :: Positive Int -> Property
prop_crossoverXMWithSameDoesNotChange d_ = forAll (resize d arbitrary :: Gen MapOfDup) $ crossoverWithSameDoesNotChange (Dup d) XM
  where d = coerce d_

prop_generateMapsCorrectSize :: Property
prop_generateMapsCorrectSize = withGenome IsDup generateMapsCorrectSize

---------------- Test Functions --------------------------------------

restrictedMovesAreNotGenerated :: [MoveDup] -> MapOfDup -> Property
restrictedMovesAreNotGenerated movs m = property $ null (genMovesWithRestriction restr m `intersect` movs)
  where restr = foldr addRestriction (emptyMoveRestriction m) movs

mutationCreateNew :: Dup -> MutMethod -> MapOfDup -> Property
mutationCreateNew d mut m = monadicIO $ do
  m' <- run . evalRandIO $ getMutation mut 1 (fromIntegral d) m
  assert $ m' /= m

crossoverWithSameDoesNotChange :: Dup -> CrossMethod -> MapOfDup -> Property
crossoverWithSameDoesNotChange d cross m = monadicIO $ do
  m' <- run . evalRandIO $ getCrossover cross 1 (fromIntegral d) m m
  assert $ m' == m

prop_dropAfterAddEmptyTabu :: Positive Int -> Positive Int -> Property
prop_dropAfterAddEmptyTabu num_ d_ =
  forAll (resize d arbitrary :: Gen MapOfDup) $ \m1 ->
  forAll (resize d arbitrary :: Gen MapOfDup) $ \m2 ->
    dropAfterAddEmptyTabu m1 m2 num
  where d = coerce d_; num = coerce num_

dropAfterAddEmptyTabu :: MapOfDup -> MapOfDup -> Int -> Property
dropAfterAddEmptyTabu m1 m2 num = property . isEmptyTabu . applyN num dropTabu $
  foldr (uncurry addTabu) (emptyTabu m1) (replicate num (m1,m2))

generateMapsCorrectSize :: (Genome g a) => g a -> Property
generateMapsCorrectSize g =
  let d = dup g in
  forAll (choose (0, if d < 7 then 2^d else 100)) $ \m ->
    monadicIO $ do
      pd <- run . runMon $ initPerm 1 Rev ""
      maps <- run . runMon $ generateMaps (makeGeneratorDup d) m
      run $ finishPerm pd
      assert $ length maps == m

return []
runTests :: IO Bool
runTests = $quickCheckAll
