{-# LANGUAGE TupleSections  #-}

module HeurMCSP where

------------------------------------------------------
import SimplifyGenomes (simplifyGenomes)
import ArgsMCSP (Args(..), processArgs, getArgs)
------------------------------------------------------
import Control.Parallel.Strategies (parMap, rdeepseq)
import System.TimeIt (timeItT)
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

  contents <- BS.readFile (input args)
  let pairs = (if singleLine args
                  then fmap (,BS.empty)
                  else toPairs) .
                      filter ((/= '#') . BS.head) . BS.lines $ contents
  let ans = if noParallel args
               then map (getNewGenomes args) pairs
               else parMap rdeepseq (getNewGenomes args) pairs
  BS.writeFile (output args) . BS.unlines . fromPairs $ ans

  where
    toPairs (s1:s2:ss) = (s1,s2):toPairs ss
    toPairs [_] = error "Odd number of lines."
    toPairs [] = []

    fromPairs ((s1,s2):ss) = s1:s2:fromPairs ss
    fromPairs [] = []

    getNewGenomes :: Args -> (BS.ByteString, BS.ByteString) -> (BS.ByteString, BS.ByteString)
    getNewGenomes args bs_genomes = simplifyGenomes bs_genomes (mcspArgs args)
