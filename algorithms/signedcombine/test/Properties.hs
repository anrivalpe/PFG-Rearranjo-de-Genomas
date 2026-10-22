module Properties where

import Test.QuickCheck

isIsomorfic :: (Eq a) => (a -> b) -> (b -> a) -> a -> Property
isIsomorfic to from x = property $ x == (from . to $ x)
