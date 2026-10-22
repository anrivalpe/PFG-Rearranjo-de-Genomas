{-# LANGUAGE ScopedTypeVariables #-}

module Test where

import GenomeCheck as GC
import DupCheck as DC
import RepCheck as RC
import AuxCheck as AC
import MCSPCheck as MCSPC
import InstanceCheck as IC
import HeuristicsCheck as HC

main :: IO ()
main = do
  ans <- fmap and . sequence $
    [ return True
    , GC.runTests
    , DC.runTests
    , RC.runTests
    , AC.runTests
    , IC.runTests
    , MCSPC.runTests
    , HC.runTests
    ]
  print ans
