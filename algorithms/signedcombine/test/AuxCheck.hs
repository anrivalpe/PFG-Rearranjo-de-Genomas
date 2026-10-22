{-# LANGUAGE TemplateHaskell, DataKinds, FlexibleInstances #-}
module AuxCheck (runTests) where

--------------------------------------------------------
import Aux
--------------------------------------------------------
import Properties
--------------------------------------------------------
import Test.QuickCheck
--------------------------------------------------------

prop_StriptoAndFromBreak :: [Int] -> [Int] -> Property
prop_StriptoAndFromBreak l1 l2 =
  forAll (sublistOf . map fromIntegral $ [1..(length l1 - 1)]) $ \idxs1 ->
  forAll (sublistOf . map fromIntegral $ [1..(length l2 - 1)]) $ \idxs2 ->
    isIsomorfic (breaks2Strips l1 l2) strips2Breaks (idxs1,idxs2)


return []
runTests :: IO Bool
runTests = $quickCheckAll
