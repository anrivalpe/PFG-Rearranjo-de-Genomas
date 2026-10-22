{-# LANGUAGE TemplateHaskell, DataKinds, FlexibleInstances #-}
module MapProperties where

--------------------------------------------------------
import Mapping
import Genome (Genome)
import Perm (permInerList, renumberToSort)
--------------------------------------------------------
import Properties (isIsomorfic)
--------------------------------------------------------
import Test.QuickCheck
--------------------------------------------------------

differentMappings :: (Genome g a, Gmap m, Eq m) => m -> m -> g a -> Property
differentMappings m1 m2 g = m1 /= m2 ==> mapGenome m1 g /= mapGenome m2 g

permMapIso :: (Genome g a, Gmap m, Eq m) => g a -> m -> Property
permMapIso g = isIsomorfic (`mapGenome` g) (`recoverMap` g)

combineEquivalency :: (Genome g a, Gmap m, GmapGenerator gen m) => gen -> g a -> g a -> m -> m -> m -> Property
combineEquivalency gen g1 g2 m1 m2 m0 = property $ p12 == p30
  where
    m3 = combineMaps gen m1 m2
    p12 = renumberToSort (mapAndList m1 g1) (mapAndList m2 g2)
    p30 = renumberToSort (mapAndList m3 g1) (mapAndList m0 g2)
    mapAndList m g = permInerList . mapGenome m $ g


return []
runTests :: IO Bool
runTests = $quickCheckAll
