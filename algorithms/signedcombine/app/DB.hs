{-# LANGUAGE TupleSections #-}

module DB where

import Types
import Genome (randomGenomeWithDuplicates, randomGenomeWithReplicas, randomGenome, shuffleGenome, toBStringOfGenes, Gene, UGl, SGl, Gl)
import Aux (Rseq, rearrange, AnyOp(..), RevOp(..), TransOp(..))
import ArgsDB (Args(..), Parameters(..), RepDB(..), DupDB(..), RandDB(..), getArgs)
---------------------------------------------------------
import Control.Monad (replicateM)
import Control.Monad.Random (getRandomR, getRandoms)
import Data.List (unfoldr)
---------------------------------------------------------
import qualified Data.ByteString.Lazy.Char8 as BS

main :: IO ()
main = do
  args <- getArgs
  genDB args

genDB :: Args -> IO ()
genDB (Args db numOfPairs n r porc sign dbtype out) = case db of
  (DB1 (DupDB d)) ->
    BS.writeFile out . BS.unlines =<<
      if sign
         then map toBStringOfGenes <$> (runMon $ genomesDup d :: IO [SGl])
         else map toBStringOfGenes <$> (runMon $ genomesDup d :: IO [UGl])
  (DB2 (RepDB l h d)) ->
    BS.writeFile out . BS.unlines =<<
      if sign
         then map toBStringOfGenes <$> (runMon $ genomesRep l h d :: IO [SGl])
         else map toBStringOfGenes <$> (runMon $ genomesRep l h d :: IO [UGl])
  (DB3 (RandDB lim)) ->
    BS.writeFile out . BS.unlines =<<
      if sign
         then map toBStringOfGenes <$> (runMon $ genomesRand lim :: IO [SGl])
         else map toBStringOfGenes <$> (runMon $ genomesRand lim :: IO [UGl])
  where
    genomesDup :: (Gene a) => Dup -> Mon [Gl a]
    genomesDup d = do
      g2s <- replicateM numOfPairs $ randomGenomeWithDuplicates n d dbtype True
      g1s <- if r == -1
        then replicateM numOfPairs $ randomGenomeWithDuplicates n d dbtype False
        else (do {ops <- operations; return $ zipWith rearrange ops g2s})
      return . concat $ zipWith (\x y -> [x,y]) g1s g2s

    genomesRep :: (Gene a) => Int -> Int -> Rep -> Mon [Gl a]
    genomesRep l h d = do
      g2s <- replicateM numOfPairs $ randomGenomeWithReplicas n d l h True dbtype 
      g1s <- if r == -1 
        then replicateM numOfPairs $ randomGenomeWithReplicas n d l h False dbtype 
        else (do {ops <- operations; return $ zipWith rearrange ops g2s})
      return . concat $ zipWith (\x y -> [x,y]) g1s g2s

    genomesRand :: (Gene a) => Int -> Mon [Gl a]
    genomesRand lim = do
      g2s <- replicateM numOfPairs $ randomGenome n lim
      g1s <- if r == -1 
        then mapM shuffleGenome g2s
        else (do {ops <- operations; return $ zipWith rearrange ops g2s})
      return . concat $ zipWith (\x y -> [x,y]) g1s g2s

    r_r = (r * porc) `div` 100
    r_t = r - r_r
    operations :: Mon [Rseq]
    operations = do
      coins <- getRandoms
      replicateM numOfPairs . sequence $ unfoldr operations_for_one (r_t, r_r, coins)
    operations_for_one :: (Int, Int, [Bool]) -> Maybe (Mon AnyOp, (Int,Int,[Bool]))
    operations_for_one (_,_,[]) = Nothing
    operations_for_one (r_t',r_r',coin:coins)
      | r_t' == 0 && r_r' == 0 = Nothing
      | r_t' == 0 || r_r' /= 0 && coin = Just . (,(r_t',r_r' - 1,coins)) $ do
          i <- getRandomR (1, fromIntegral n)
          j <- getRandomR (i, fromIntegral n)
          return . Op1 $ R (Idx i) (Idx j)
      | otherwise = Just . (,(r_t' - 1, r_r',coins)) $ do
          i <- getRandomR (1, fromIntegral n-1)
          j <- getRandomR (i+1, fromIntegral n)
          k <- getRandomR (j+1, fromIntegral n+1)
          return . Op2 $ T (Idx i) (Idx j) (Idx k)
