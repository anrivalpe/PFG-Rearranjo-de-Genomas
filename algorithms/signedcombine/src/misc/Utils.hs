{-# LANGUAGE TupleSections #-}
module Utils where

-------------------------
import Control.Arrow (second)
import Control.Monad.Trans.State.Strict (StateT(..))
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import Data.Tuple (swap)
-------------------------
import Data.List as List
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set
import qualified Data.Map as Map
-------------------------

sign :: (Num n, Ord n) => n -> n
-- get sing of an element
sign x = if x < 0 then -1 else 1

lPairs :: [a] -> [(a,a)]
-- get all consecutive pairs of a list
lPairs l = zip l (tail l)

lTriples :: [a] -> [(a,a,a)]
-- get all consecutive triples of a list
lTriples l = zip3 l (tail l) (tail . tail $ l)

avg :: (Foldable f, Real a) => f a -> Double
-- calculate the average
avg l = if n == 0 then 0 else s / n
  where
    n = fromIntegral . length $ l
    s :: Double
    s = realToFrac . sum $ l

stdev :: (Foldable f, Real a) => f a -> Double
-- calculate the standard deviation
stdev l = sqrt . avg . map ((^ (2 :: Int)) . (-) mi . realToFrac) . toList $ l
  where mi = avg l

uniqueSort :: (Ord a) => [a] -> [a]
-- sort and eliminate duplicates of a list
uniqueSort = Set.toList . Set.fromList

enumToNum :: (Enum e, Num n) => e -> n
-- convert Enum types
enumToNum = fromIntegral . fromEnum

traceValue :: (Show a) => a -> a
-- trace for debug
traceValue = traceValueS ""

traceValueS :: (Show a) => String -> a -> a
-- trace for debug, with prefix string
traceValueS str val = trace (str ++ " ---" ++ show val ++ "---") val

fdiv :: (Real a, Real b) => a -> b -> Double
-- division between possible distinct real types
fdiv x y = realToFrac x / realToFrac y

minWith :: (Foldable t, Ord b) => (a -> b) -> t a -> a
-- calculate minimum converting values with function
minWith f = List.minimumBy (\x y -> compare (f x) (f y))

maxWith :: (Foldable t, Ord b) => (a -> b) -> t a -> a
-- calculate maximum converting values with function
maxWith f = List.maximumBy (\x y -> compare (f x) (f y))

listToMaybeList :: [a] -> Maybe [a]
-- Return Nothing if list is empty
listToMaybeList l = if null l then Nothing else Just l

applyN :: Int -> (a -> a) -> a -> a
-- apply function n times
applyN n f = foldr (.) id (replicate n f)

both :: (a -> b) -> (a,a) -> (b,b)
-- apply function to both elements of a pair
both f (a1,a2) = (f a1,f a2)

mapAccumM :: (Monad m, Traversable t) => (a -> b -> m (a, c)) -> a -> t b -> m (a, t c)
-- monadic version of mapAccumM
mapAccumM f_ s t = swap <$> runStateT (traverse (StateT . f) t) s
  where f a b = swap <$> f_ b a

nothingIsMin :: (Bounded a) => Maybe a -> a
-- replace Nothing if lowest value possible
nothingIsMin = fromMaybe minBound

nothingIsMax :: (Bounded a) => Maybe a -> a
-- replace Nothing if highest value possible
nothingIsMax = fromMaybe maxBound

fac :: (Integral a, Integral b) => a -> b
-- factorial of a number
fac n = product [1..fromIntegral n]

showIntList :: [Int] -> BS.ByteString
showIntList = BSB.toLazyByteString . showList'
  where
    showList' [] = BSB.char8 '['  <> BSB.char8 ']'
    showList' (x:xs) = BSB.char8 '[' <> BSB.intDec x <> go xs
     where
       go (y:ys) = BSB.char8 ',' <> BSB.intDec y <> go ys
       go [] = BSB.char8 ']'

countReplicas :: (Ord a) => [a] -> [(a,Int)]
countReplicas = Map.assocs . Map.fromListWith (+) . map (,1)

separateByKey :: (Ord k) => [(k,v)] -> [(k,[v])]
-- separete by key, result is sorted
separateByKey = Map.assocs . Map.fromListWith (flip (++)) . map (second (:[]))

contractNumbers :: [Int] -> [Int]
-- renumber to use numbers from [1..]
contractNumbers l = map (\x -> let el = m IntMap.! (fromIntegral . abs $ x) in if x >= 0 then el else -el) l
  where m = IntMap.fromList $ zip (uniqueSort $ fromIntegral . abs <$> l) [1..]

findSublistIndex :: Eq a => [a] -> [a] -> Maybe Int
findSublistIndex ys xs = findIndex (isPrefixOf ys) $ tails xs
