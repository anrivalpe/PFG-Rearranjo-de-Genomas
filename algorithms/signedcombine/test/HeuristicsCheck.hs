{-# LANGUAGE TemplateHaskell, TypeFamilies, ExplicitForAll, TypeApplications, ScopedTypeVariables #-}
module HeuristicsCheck (runTests) where

-------------------------------------------------------------------------
import Types
import FindDistance
-------------------------------------------------------------------------
import Args(Commands(..), CrossMethod(..), MutMethod(..), RM(..), Local(..), Tabu(..), Cuckoo(..), Grasp(..), GenAlg(..), MemAlg(..), SimAnn(..), Sep(..), GenBP(..), Ext(..), Soar(..), PermDist(..), HS(..), MapHeur(..), SingleDist(..), Sep(..), InitialMap(..))
import Genome (Genome, toBStringOfGenes, sig, len)
import Instance (ReaInstGenMod, unwrapInstance, dup, rep, RepStatus(..))
import Perm (initPerm, finishPerm)
import Aux (model)
-------------------------------------------------------------------------
import InstanceCheck (withRearrangeInstance, withRearrangeInstance')
import GenomeCheck (withGenome)
-------------------------------------------------------------------------
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)
-------------------------------------------------------------------------
import qualified ArgsMCSP as MCSP
-------------------------------------------------------------------------

int2model :: Int -> RearrangeModel
int2model i = toEnum . abs $ i `mod` 3

---------------- Properties --------------------------------------

prop_permDistPositive :: Bool -> Property
prop_permDistPositive contNei = withRearrangeInstance IsPerm $ distPositive False contNei (SD $ A0 PermDist) 1

prop_permDistZero :: Int -> Bool -> Property
prop_permDistZero model_int contNei = withGenome IsPerm $ distZero False contNei (int2model model_int) (SD $ A0 PermDist) 1

prop_mapDistPositive :: Bool -> Property
prop_mapDistPositive contNei = withRearrangeInstance IsStr $ distPositive False contNei (MH $ A2 RM) 3

prop_mapDistNonnegative :: Bool -> Property
prop_mapDistNonnegative contNei = withRearrangeInstance IsStr $ distNonNegative False contNei (MH $ A2 RM) 3

prop_localDistPositive :: Bool -> Property
prop_localDistPositive contNei = withRearrangeInstance IsStr $ \inst ->
  let d = rep inst in (d /= 0) ==>
    let r = if d < 4 then 2^d else 10 in
    distPositive False contNei (MH $ A4 (Local 5 2)) r inst

prop_localDistNonnegative :: Bool -> Property
prop_localDistNonnegative contNei = withRearrangeInstance IsStr $ \inst ->
  let d = rep inst in (d /= 0) ==>
    let r = if d < 4 then 2^d else 10 in
    distNonNegative False contNei (MH $ A4 (Local 5 2)) r inst

prop_tabuDistPositive :: Property
prop_tabuDistPositive = withRearrangeInstance IsStr $ \inst ->
  let d = fromIntegral $ rep inst in
      (d /= 0) ==> distPositive False False (MH $ A7 (Tabu 2 2)) 10 inst

prop_tabuDistNonnegative :: Property
prop_tabuDistNonnegative = withRearrangeInstance IsStr $ \inst ->
  let d = fromIntegral $ rep inst in
      (d /= 0) ==> distNonNegative False False (MH $ A7 (Tabu 2 2)) 10 inst

prop_cuckooDistPositive :: Bool -> Property
prop_cuckooDistPositive contNei = withRearrangeInstance IsStr $ \inst ->
  distPositive False contNei (MH $ A14 (Cuckoo 2 1 10)) 50 inst

prop_cuckooDistNonnegative :: Bool -> Property
prop_cuckooDistNonnegative contNei = withRearrangeInstance IsStr $ \inst ->
  distNonNegative False contNei (MH $ A14 (Cuckoo 2 1 10)) 50 inst

prop_graspDistPositive :: Bool -> Bool -> Property
prop_graspDistPositive contNei simAnn = withRearrangeInstance IsStr $ distPositive False contNei (MH $ A5 (Grasp 5 5 5 5 simAnn)) 30

prop_graspDistNonnegative :: Bool -> Bool -> Property
prop_graspDistNonnegative simAnn contNei = withRearrangeInstance IsStr $ distNonNegative False contNei (MH $ A5 (Grasp 5 5 5 5 simAnn)) 30

prop_genAlgDistPositive :: Bool -> Property
prop_genAlgDistPositive contNei = withRearrangeInstance IsStr $ distPositive False contNei (MH $ A3 (GenAlg 5 5 2 2 2 2 MR XM 2 2)) 33

prop_genAlgDistNonnegative :: Bool -> Property
prop_genAlgDistNonnegative contNei = withRearrangeInstance IsStr $ distNonNegative False contNei (MH $ A3 (GenAlg 5 5 2 2 2 2 MR XM 2 2)) 33

prop_memAlgDistPositive :: Bool -> Bool -> Property
prop_memAlgDistPositive simAnn contNei = withRearrangeInstance IsStr $ distPositive False contNei (MH $ A15 (MemAlg 5 5 2 2 XM 2 2 simAnn)) 33

prop_memAlgDistNonnegative :: Bool -> Bool -> Property
prop_memAlgDistNonnegative simAnn contNei = withRearrangeInstance IsStr $ distNonNegative False contNei (MH $ A15 (MemAlg 5 5 2 2 XM 2 2 simAnn)) 33

prop_simAnnDistPositive :: Property
prop_simAnnDistPositive = withRearrangeInstance IsStr $ \inst ->
  let d = dup inst in
    (d /= 0) ==> distPositive False False (MH $ A8 (SimAnn 10 2 1 50)) 100 inst

prop_simAnnDistNonnegative :: Property
prop_simAnnDistNonnegative = withRearrangeInstance IsStr $ \inst ->
  let d = dup inst in
    (d /= 0) ==> distNonNegative False False (MH $ A8 (SimAnn 10 2 1 50)) 100 inst

-- prop_sepDistPositive :: Property
-- prop_sepDistPositive = withRearrangeInstance IsDup $ \inst ->
--   let d = fromIntegral $ dup inst in
--     (d /= 0) ==> distPositive True False (MH $ A9 Sep) (max d 10) inst

-- prop_sepDistNonnegative :: Property
-- prop_sepDistNonnegative = withRearrangeInstance IsDup $ \inst ->
--   let d = fromIntegral $ dup inst in
--     (d /= 0) ==> distNonNegative True False (MH $ A9 Sep) (max d 10) inst

extDistPositive :: Bool -> Property
extDistPositive contNei = withRearrangeInstance' True IsStr $ \inst ->
  distPositive False contNei (SD $ A1 (Ext True)) 1 inst

extDistNonnegative :: Bool -> Property
extDistNonnegative contNei = withRearrangeInstance' True IsStr $ \inst ->
  distNonNegative False contNei (SD $ A1 (Ext True)) 1 inst

prop_genBPDistPositive :: Bool -> Bool -> Property
prop_genBPDistPositive b contNei = withRearrangeInstance IsStr $ \inst ->
  distPositive False contNei (SD $ A12 (GenBP b False)) 1 inst

prop_genBPDistNonnegative :: Bool -> Bool -> Property
prop_genBPDistNonnegative b contNei = withRearrangeInstance IsStr $ \inst ->
  distNonNegative False contNei (SD $ A12 (GenBP b False)) 1 inst

prop_soarDistPositive :: Bool -> Property
prop_soarDistPositive contNei = withRearrangeInstance IsStr $ distPositive False contNei (SD $ A6 (Soar Ap4 False False)) 1

prop_soarDistNonnegative :: Bool -> Property
prop_soarDistNonnegative contNei = withRearrangeInstance IsStr $ distNonNegative False contNei (SD $ A6 (Soar Ap4 False False)) 1

prop_hsDistPositive :: Bool -> Property
prop_hsDistPositive contNei = withRearrangeInstance IsStr $ distPositive False contNei (SD $ A11 HS) 1

prop_hsDistNonnegative :: Bool -> Property
prop_hsDistNonnegative contNei = withRearrangeInstance IsStr $ distNonNegative False contNei (SD $ A11 HS) 1

---------------- Test Functions --------------------------------------

getDist :: (Genome g a) => Bool -> Bool -> Bool -> RearrangeModel -> g a -> g a -> Commands -> Int -> IO [Dist]
getDist dup contNei useAss model g1 g2 com total = do
    let s = sig g1
    pd <- case com of
      (SD (A6 _)) -> runMon $ initPerm 1 model ""
      (SD (A11 _)) -> runMon $ initPerm 1 model ""
      _ -> runMon $ initPerm 1 model ""
    dist <- runMon $ findDistance False (toBStringOfGenes g1, toBStringOfGenes g2) pd (if s then Signed else Unsigned, model, com, total, dup, WithRand, contNei, useAss, MCSP.P0 MCSP.ID)
    finishPerm pd
    return dist

distNonNegative :: forall inst g a mod. (ReaInstGenMod inst g a mod) => Bool -> Bool -> Commands -> Int -> inst -> Property
distNonNegative dup contNei com total inst =
  let (g1, g2, _, _, _) = unwrapInstance inst in
  forAll arbitrary $ \useAss ->
  (len g1 /= 0) ==>
  monadicIO $ do
    dist <- run $ getDist dup contNei useAss (model @mod) g1 g2 com total
    assert . all (>= 0) $ dist

distPositive :: forall inst g a mod. (ReaInstGenMod inst g a mod) => Bool -> Bool -> Commands -> Int -> inst -> Property
distPositive dup contNei com total inst =
  let (g1, g2, _, _, _) = unwrapInstance inst in
  forAll arbitrary $ \useAss ->
  (g1 /= g2) ==>
  monadicIO $ do
    dist <- run $ getDist dup contNei useAss (model @mod) g1 g2 com total
    assert . all (> 0) $ dist

distZero :: (Genome g a) => Bool -> Bool -> RearrangeModel -> Commands -> Int -> g a -> Property
distZero dup contNei model com total g =
    forAll arbitrary $ \useAss ->
    monadicIO $ do
    dist <- run $ getDist dup contNei useAss model g g com total
    assert . all (== 0) $ dist

return []

runTests :: IO Bool
runTests = do
  print "=== test Ext ==="
  quickCheckWith stdArgs{maxSize = 60} extDistPositive
  quickCheckWith stdArgs{maxSize = 60} extDistNonnegative
  $quickCheckAll
