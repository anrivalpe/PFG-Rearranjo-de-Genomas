{-# LANGUAGE ScopedTypeVariables, TupleSections, TypeApplications, FlexibleContexts, GADTs, Rank2Types #-}

module SimplifyGenomes (simplifyGenomes, partitionIsSigned) where

------------------------------------------------------
import Types
------------------------------------------------------
import MCSP (ContractedInstance(..), assigments, mcspSOARWithBreak, mcspHS, mcspCombine, mcspGreedy)
import ArgsMCSP(Commands(..), MCSPArgs)
import Genome (Gene, SGene, UGene, GenomeType(..))
------------------------------------------------------
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Proxy (Proxy(..))
------------------------------------------------------
import qualified Instance as I
import qualified ArgsMCSP as MCSP
------------------------------------------------------

-- Returns Signed if the simplified genome will be signed or Unsigned otherwise
partitionIsSigned :: MCSPArgs -> IsSign
partitionIsSigned (MCSP.Direct, _, _, _) = Unsigned
partitionIsSigned (MCSP.Reverse, (P0 _), _, _) = Unsigned
partitionIsSigned (MCSP.Reverse, _, _, _) = Signed
partitionIsSigned (MCSP.Signed, _, _, _) = Signed

-- Simplify the genomes using some partition algoritm
-- Arguments:
--      bs_genomes: the genomes
--      variation: with variation of the MCSP problem should be consider
--      any_com: information to select and configure the algorithm
--      onlyDup: weather the genomes only have duplicates
--      useAss: whether to apply the initial assigments
simplifyGenomes :: (ByteString, ByteString) -> MCSPArgs -> (ByteString, ByteString)
simplifyGenomes bs_genomes (_, (P0 _), _, False) = bs_genomes
simplifyGenomes bs_genomes (variation, any_com, onlyDup, useAss) =
  case variation of
    MCSP.Direct -> simplifyGenomes_ Signed Trans (Proxy @SGene)
    MCSP.Reverse -> simplifyGenomes_ Signed Rev (Proxy @SGene)
    MCSP.Signed -> simplifyGenomes_ Unsigned Rev (Proxy @UGene)
  where
    simplifyGenomes_ :: forall a. (Gene a) => IsSign -> RearrangeModel -> Proxy a -> (ByteString, ByteString)
    simplifyGenomes_ isSign model _ =
        I.writeInstance . mcsp . (if useAss then assigments else id) $ 
        if onlyDup
           then I.readCorrectInstance I.IsDup GList isSign model bs_genomes 
           else I.readCorrectInstance I.IsStr GList isSign model bs_genomes 
    mcsp :: I.AnyInstance -> I.AnyInstance
    mcsp (I.AnyInstance inst) = contractedInst $
        case any_com of
             (P0 _) -> ContInst (I.AnyInstance inst) ([],[])
             (P1 com) -> mcspSOARWithBreak com inst
             (P2 com) -> mcspHS com False inst
             (P3 com) -> mcspCombine com inst
             (P4 com) -> mcspGreedy com inst
