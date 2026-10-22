{-# LANGUAGE TupleSections  #-}

module HeurGRDG where

import Utils
import Types
------------------------------------------------------
import FindDistance (findDistance)
import Args(Args(..), processArgs, getArgs)
import Perm (initPerm, finishPerm)
------------------------------------------------------
import Control.Concurrent.ParallelIO.Global (parallel, stopGlobalPool)
import System.TimeIt (timeItT)
import Data.Coerce (coerce)
------------------------------------------------------
import qualified Data.ByteString.Lazy.Char8 as BS
------------------------------------------------------

main :: IO ()
main = do
    args <- processArgs <$> getArgs -- command line arguments
    (time,_) <- timeItT (main_ args)

    -- Write line with running time.
    let time_info = "# CPU Time: " ++ show (time) ++ "s"
    appendFile (output args) time_info

main_ :: Args -> IO ()
main_ args = do

  -- Input:
  contents <- BS.readFile (input args) -- read input file
  -- Transform input into pairs of strings.
  let pairs = (if singleLine args
                  then fmap (,BS.empty) -- instance in single line (for sorting problems)
                  else toPairs) .
                      -- Separate lines, lines starting with # are ignored.
                      filter ((/= '#') . BS.head) . BS.lines $ contents

  -- Solve:
  ans <- if noParallel args
        then mapM (getDists args) pairs
        else do -- get each distance in parallel
          ans <- parallel $ map (getDists args) pairs
          stopGlobalPool
          return ans

  -- Output:
  BS.writeFile (output args) . BS.unlines . map showIntList $ ans

  where
    toPairs (s1:s2:ss) = (s1,s2):toPairs ss
    toPairs [_] = error "Odd number of lines."
    toPairs [] = []

    getDists :: Args -> (BS.ByteString, BS.ByteString) -> IO [Int]
    getDists args bs_genomes = do
      pd <- runMon $ initPerm (permId args) (modelArg args) (permProg args)
      dists <- runMon . fmap coerce $ findDistance False bs_genomes pd (distArgs args)
      finishPerm pd
      return $
          if getMin args
             then (:[]) . minimum $ dists -- return best upper bounds
             else dists -- return all upper bounds
