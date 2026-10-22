{-# LANGUAGE TemplateHaskell, DataKinds, FlexibleInstances, TypeFamilies, FlexibleContexts #-}
module MCSPCheck (runTests) where

--------------------------------------------------------
import MCSP
--------------------------------------------------------
import Instance (ReaInstAll, getSource, getTarget, RepStatus(..))
import Mapping (Gmap)
--------------------------------------------------------
import Properties (isIsomorfic)
import InstanceCheck (withRearrangeInstance)
import RepCheck (genMapOfGenome)
--------------------------------------------------------
import Test.QuickCheck
--------------------------------------------------------

prop_contractedMapIso :: Property
prop_contractedMapIso = withRearrangeInstance IsStr $ \inst ->
    let g1 = getSource inst
        g2 = getTarget inst in
    forAll (genMapOfGenome g1) $ \m1 ->
    forAll (genMapOfGenome g2) $ \m2 ->
        contractedMapIso inst m2 m1

contractedMapIso :: (ReaInstAll inst g a a' mod, Gmap m, Eq m) => inst -> m -> m -> Property
contractedMapIso inst m2 = isIsomorfic (contractMap inst m2) (uncurry uncontractMap)

return []
runTests :: IO Bool
runTests = $quickCheckAll
