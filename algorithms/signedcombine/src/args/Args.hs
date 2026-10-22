{-# LANGUAGE FlexibleContexts #-}

module Args where

------------------------------------------------------
import Types
------------------------------------------------------
import Options.Applicative (Parser, ParserInfo, option, auto, long, short, metavar, help, value, showDefault, switch, subparser, command, info, (<**>), helper, progDesc, strOption, fullDesc, execParser, (<|>), commandGroup)
import Control.Exception.Base (throw)
import Control.Monad.IO.Class (MonadIO, liftIO)
------------------------------------------------------
import qualified ArgsMCSP as MCSP
------------------------------------------------------

data Commands = MH MapHeur | SD SingleDist
data MapHeur = A2 RM | A3 GenAlg | A4 Local | A5 Grasp | A7 Tabu |
               A8 SimAnn | A9 Sep | A14 Cuckoo | A15 MemAlg | BX CuckooMeta | CX SepMeta
data SingleDist = A0 PermDist | A1 Ext |A6 Soar | A11 HS | A12 GenBP | A16 Cycle
data PermDist = PermDist
data RM = RM
data Local = Local {
    cl_initial :: Int
  , cl_each :: Int
  }
data Tabu = Tabu {
    ct_each :: Int
  , ct_tabu_size :: Int
  }
data Cuckoo = Cuckoo {
    cc_initial :: Int
  , cc_eliminate :: Int
  , cc_flip_t :: Int
  }
data CuckooMeta = CuckooMeta {
    ccm_initial :: Int
  , ccm_eliminate :: Int
  , ccm_com :: MapHeur
  }
data Grasp = Grasp {
    cw_initial :: Int
  , cw_create_rlc :: Int
  , cw_create_loc :: Int
  , cw_rcl_size :: Int
  , cw_sim_ann :: Bool
  }
data GenAlg = GenAlg {
    cg_initial :: Int
  , cg_keep :: Int
  , cg_lom :: Int
  , cg_him :: Int
  , cg_lox :: Int
  , cg_hix :: Int
  , cg_mut_op :: MutMethod
  , cg_cross_op :: CrossMethod
  , cg_gm :: Int
  , cg_gc :: Int
  }
data MemAlg = MemAlg {
    cm_initial :: Int
  , cm_keep :: Int
  , cm_lox :: Int
  , cm_hix :: Int
  , cm_cross_op :: CrossMethod
  , cm_gc :: Int
  , cm_create_loc :: Int
  , cm_sim_ann :: Bool
  }
data SimAnn = SimAnn {
    cl_max_iter :: Int
  , cs_temp_initial :: Int
  , cs_temp_constant :: Double
  , cs_change_rate :: Int
  }
data Sep = Sep
data SepMeta = SepMeta {
    sm_initial :: Int
  , sm_level :: Int
  , sm_mh :: MapHeur
  }
newtype Ext = Ext {
    ex_use_all :: Bool
  }
data Soar = Soar {
    so_aprox :: Aprox
  , so_noheu :: Bool
  , so_rperm :: Bool
  } deriving (Eq)
data HS = HS
data Cycle = Cycle
data GenBP = GenBP {
    bp_trans_first :: Bool
  , bp_ext :: Bool}

data MutMethod = MR | MB deriving (Read, Eq, Show)

data CrossMethod = XR | XB | XP | XM deriving (Read, Eq, Show)

data WeightFun = Id | Root2 | Root3 | Sigm10 | Sigm50 deriving (Read, Show)

data InitialMap = WithCycle | WithSoar | WithRand deriving (Read, Show)

---------------------- HeurGRDG --------------------------

data ArgsRaw = ArgsRaw
  { raw_permId :: Int
  , raw_permProg :: String
  , raw_reMod :: RearrangeModel
  , raw_isSign :: Bool
  , raw_createTotal :: Int
  , raw_input :: String
  , raw_output :: String
  , raw_onlyDup :: Bool
  , raw_getMin :: Bool
  , raw_noPar :: Bool
  , raw_singleLine :: Bool
  , raw_withMap :: InitialMap
  , raw_contNei :: Bool
  , raw_useAss :: Bool
  , raw_com :: Commands
  , raw_mcspCom :: MCSP.Commands
  }

type DistArgs = (IsSign, RearrangeModel, Commands, Int, Bool, InitialMap, Bool, Bool, MCSP.Commands)
data Args = Args {
    distArgs :: DistArgs
  , modelArg :: RearrangeModel
  , permId :: Int
  , permProg :: String
  , getMin :: Bool
  , input :: String
  , output :: String
  , noParallel :: Bool
  , singleLine :: Bool
  }

permParser :: Parser PermDist
permParser = pure PermDist

mapParser :: Parser RM
mapParser = pure RM

localParser :: Parser Local
localParser = Local
      <$> option auto
          ( long "initial"
         <> short 'c'
         <> metavar "INI"
         <> help "Number of mappnigs to start with" )
      <*> option auto
          ( long "local_create"
         <> short 'l'
         <> metavar "LOC"
         <> help "Number of mappings to create in each local search." )

tabuParser :: Parser Tabu
tabuParser = Tabu
      <$> option auto
          ( long "create-each"
         <> short 'l'
         <> metavar "EACH"
         <> help "Number of mappings to create in each local search." )
      <*> option auto
          ( long "tabu"
         <> short 't'
         <> metavar "TABU"
         <> help "Total number of interation to keep a mov on the tabu list." )

cuckooParser :: Parser Cuckoo
cuckooParser = Cuckoo
      <$> option auto
          ( long "initial"
         <> short 'c'
         <> metavar "INI"
         <> help "Number of mappings to start with." )
      <*> option auto
          ( long "create-each"
         <> short 'e'
         <> metavar "EACH"
         <> help "Number of mappings to create in each generation." )
      <*> option auto
          ( long "prob"
         <> short 'p'
         <> metavar "BITS"
         <> help "Probability to stop when flipping bits." )

cuckooMetaParser :: Parser CuckooMeta
cuckooMetaParser = CuckooMeta
      <$> option auto
          ( long "initial"
         <> short 'c'
         <> metavar "INI"
         <> help "Number of nests." )
      <*> option auto
          ( long "create-each"
         <> short 'e'
         <> metavar "EACH"
         <> help "Number of generations until eliminate a nest." )
      <*> mhParser

graspParser :: Parser Grasp
graspParser = Grasp
      <$> option auto
          ( long "initial"
         <> short 'c'
         <> metavar "INI"
         <> help "Number of mappings to start with." )
      <*> option auto
          ( long "generate_rcl"
         <> short 'g'
         <> metavar "CRE"
         <> help "Number of new mappings to create in each generation, using rcl." )
      <*> option auto
          ( long "rl"
         <> metavar "CRE"
         <> help "Number of new mappings to create in each generation, using local search." )
      <*> option auto
          ( long "rcl"
         <> short 'k'
         <> metavar "RCL"
         <> help "Number of mappings to use on RCL.")
      <*> switch
          ( long "use-simann"
         <> help "Whether to use Simulated Annealing as the local search.")

genAlgParser :: Parser GenAlg
genAlgParser = GenAlg
      <$> option auto
          ( long "initial"
         <> short 'c'
         <> metavar "INI"
         <> help "Number of chromosomes to start with." )
      <*> option auto
          ( long "keep"
         <> short 'k'
         <> metavar "KEEP"
         <> help "Number of chromosomes to keep in each generation." )
      <*> option auto
          ( long "low_mutation"
         <> short 'm'
         <> metavar "LO"
         <> help "Lowest value for mutation." )
      <*> option auto
          ( long "high_mutation"
         <> short 'M'
         <> metavar "HI"
         <> value (-1)
         <> help "Highest value for mutation. (defaut: low-mutation)" )
      <*> option auto
          ( long "low_crossover"
         <> short 'x'
         <> metavar "LO"
         <> help "Lowest value for crossover." )
      <*> option auto
          ( long "high_cross"
         <> short 'X'
         <> metavar "HI"
         <> value (-1)
         <> help "Highest value for crossover. (defaut: low-crossover)" )
      <*> option auto
          ( long "mut"
         <> metavar "MUT"
         <> showDefault
         <> value MR
         <> help "Mutation to use.")
      <*> option auto
          ( long "cross"
         <> metavar "CRO"
         <> showDefault
         <> value XM
         <> help "Crossover to use.")
      <*> option auto
          ( long "gm"
         <> value (-1)
         <> metavar "GM"
         <> help "Number of new chromosomes to create with mutation. (defaut: keep)" )
      <*> option auto
          ( long "gc"
         <> value (-1)
         <> metavar "GC"
         <> help "Number of new chromosomes to create with crossover. (defaut: keep / 2)" )

memAlgParser :: Parser MemAlg
memAlgParser = MemAlg
      <$> option auto
          ( long "initial"
         <> short 'c'
         <> metavar "INI"
         <> help "Number of maps to start with." )
      <*> option auto
          ( long "keep"
         <> short 'k'
         <> metavar "KEEP"
         <> help "Number of maps to keep in each generation." )
      <*> option auto
          ( long "low_crossover"
         <> short 'x'
         <> metavar "LO"
         <> help "Lowest value for crossover." )
      <*> option auto
          ( long "high_cross"
         <> short 'X'
         <> metavar "HI"
         <> value (-1)
         <> help "Highest value for crossover. (defaut: low-crossover)" )
      <*> option auto
          ( long "cross"
         <> metavar "CRO"
         <> showDefault
         <> value XM
         <> help "Crossover to use.")
      <*> option auto
          ( long "gc"
         <> value (-1)
         <> metavar "GC"
         <> help "Number of new chromosomes to create with crossover. (defaut: keep / 2)" )
      <*> option auto
          ( long "rl"
         <> metavar "CRE"
         <> help "Number of new mappings to create in each generation, using local search." )
      <*> switch
          ( long "use-simann"
         <> help "Whether to use Simulated Annealing as the local search.")

simAnnParser :: Parser SimAnn
simAnnParser = SimAnn
      <$> option auto
          ( long "max_iter"
         <> short 'i'
         <> metavar "MAX"
         <> help "Maximum number of iteration without improvement for each temperature." )
      <*> option auto
          ( long "temp_initial"
         <> short 't'
         <> metavar "TEMP0"
         <> help "Initial value of temperature." )
      <*> option auto
          ( long "temp_constant"
         <> short 'k'
         <> metavar "CONST"
         <> help "Multiplicative constant of temperature." )
      <*> option auto
          ( long "change_rate"
         <> short 'a'
         <> metavar "CONST"
         <> help "Rate of change (percentage) for temperature." )

sepParser :: Parser Sep
sepParser = pure Sep

sepMetaParser :: Parser SepMeta
sepMetaParser = SepMeta
      <$> option auto
          ( long "initial"
         <> short 'c'
         <> metavar "INI"
         <> help "Number of maps to cerate with sep." )
      <*> option auto
          ( long "level"
         <> short 'l'
         <> metavar "LEV"
         <> help "Number of bits to fix in sep." )
      <*> mhParser

extParser :: Parser Ext
extParser = Ext
      <$> switch
          ( long "use_all_revs"
         <> help "Teste all possible set of 1 or 2 reversals that position a element in an extremity.")

soarParser :: Parser Soar
soarParser = Soar
      <$> option auto
          ( long "approximation"
         <> short 'a'
         <> metavar "AP"
         <> value Ap4
         <> help "Aproximation algoritm to use." )
      <*> switch
          ( long "no_heuristics"
         <> short 'n'
         <> help "Do not use heuristics to improve the result.")
      <*> switch
          ( long "use_map"
         <> short 'm'
         <> help "Use a map of the original strings to calculate the distance.")

hsParser :: Parser HS
hsParser = pure HS

cycleParser :: Parser Cycle
cycleParser = pure Cycle

genBPParser :: Parser GenBP
genBPParser = GenBP
      <$> switch
          ( long "trans_first"
         <> help "Whether to prefer transposition when removing two break points." )
      <*> switch
          ( long "ext"
         <> help "Whether to use rank when positioning an elemet on the extremity." )

mhParser :: Parser MapHeur
mhParser = subparser $
          commandGroup "MAP_ALGO: Maps based metaheuristics to find rearrangement distances considering multiple genes"
          <> metavar "MAP_ALGO"
          <>  command "Map" (info
                           (A2 <$> mapParser <**> helper)
                           (progDesc "Heuristic of Random Mappings"))
          <> command "Local" (info
                           (A4 <$> localParser <**> helper)
                           (progDesc "Heuristic of Local Search"))
          <> command "Tabu" (info
                           (A7 <$> tabuParser <**> helper)
                           (progDesc "Heuristic of Tabu Search"))
          <> command "Cuckoo" (info
                           (A14 <$> cuckooParser <**> helper)
                           (progDesc "Heuristic of Cuckoo Search"))
          <> command "CuckooMeta" (info
                           (BX <$> cuckooMetaParser <**> helper)
                           (progDesc "Heuristic of Cuckoo Search combined with others"))
          <> command "Grasp" (info
                           (A5 <$> graspParser <**> helper)
                           (progDesc "Heuristic of GRASP"))
          <> command "GenAlg" (info
                           (A3 <$> genAlgParser <**> helper)
                           (progDesc "Heuristic of Genetic Algorithms"))
          <> command "MemAlg" (info
                           (A15 <$> memAlgParser <**> helper)
                           (progDesc "Heuristic of Memetic Algorithms"))
          <> command "SimAnn" (info
                           (A8 <$> simAnnParser <**> helper)
                           (progDesc "Heuristic of Simulated Annealing"))
          <> command "Sep" (info
                           (A9 <$> sepParser <**> helper)
                           (progDesc "Heuristic of Separation"))
          <> command "SepMeta" (info
                           (CX <$> sepMetaParser <**> helper)
                           (progDesc "Heuristic of Separation combined with others"))

sdParser :: Parser SingleDist
sdParser = subparser $
          commandGroup "OTHER_ALGO: Other Algorithms to find rearrangement distances"
          <> metavar "OTHER_ALGO"
          <> command "PermDist" (info
                           (A0 <$> permParser <**> helper)
                           (progDesc "Distance between permutations"))
          <> command "Ext" (info
                           (A1 <$> extParser <**> helper)
                           (progDesc "Heuristic of extremities"))
          <> command "Soar" (info
                           (A6 <$> soarParser <**> helper)
                           (progDesc "Algorithm from Soar paper"))
          <> command "HS" (info
                           (A11 <$> hsParser <**> helper)
                           (progDesc "Algorithm from Hitting Set paper"))
          <> command "Cycle" (info
                           (A16 <$> cycleParser <**> helper)
                           (progDesc "Only cycle decomposition form the SOAR algorithm"))
          <> command "GenBP" (info
                           (A12 <$> genBPParser <**> helper)
                           (progDesc "Greedy algorithms using generalized break points"))

argParser :: Parser ArgsRaw
argParser = ArgsRaw
      <$> option auto
          ( long "pid"
         <> short 'c'
         <> metavar "P_ID"
         <> value 1
         <> showDefault
         <> help "Code of algorithm to use on permutations (0 to use custom program, > 0 to use the c header)." )
      <*> strOption
          ( long "pprog"
         <> short 'p'
         <> metavar "PPROG"
         <> value ""
         <> help "Program to use with P_ID=0" )
      <*> option auto
          ( long "rearrangement-events"
         <> short 'e'
         <> metavar "EVENT"
         <> showDefault
         <> value Rev
         <> help "Rearrangement model to consider. One of Rev, Trans or TransRev.")
      <*> switch
          ( long "sign"
         <> short 's'
         <> help "Consider the Genomes signed.")
      <*> option auto
          ( long "mappings"
         <> short 'm'
         <> metavar "REP"
         <> showDefault
         <> value 100
         <> help "Total number of mappings to create." )
      <*> strOption
          ( long "input"
         <> short 'i'
         <> metavar "IFILE"
         <> help "Input file")
      <*> strOption
          ( long "outfile"
         <> short 'o'
         <> metavar "OFILE"
         <> help "Output file")
      <*> switch
          ( long "dup"
         <> short 'd'
         <> help "Genome has only duplicated Genes. Use this option to improve efficiency if you know that no gene has more than two copies" )
      <*> switch
          ( long "min"
         <> help "When using maps returns the minimum instead off all values.")
      <*> switch
          ( long "no-par"
         <> help "Do not process the genomes in parallel.")
      <*> switch
          ( long "single-line"
         <> help "Target genome is the identity. Useful when sorting permutations.")
      <*> option auto
          ( long "with-map"
         <> value WithRand
         <> help "Use cycle decomposition or soar to get initial map. Recommended if there are genes with many copies.")
      <*> switch
          ( long "cont-neig"
         <> help "Use alternative definition of neighbors.")
      <*> switch
          ( long "ass"
         <> help "Use the initial assignments. May improve the result if many replicated genes are present." )
      <*> ( (MH <$> mhParser) <|> (SD <$> sdParser))
      <*> MCSP.mcspParser

opts :: ParserInfo ArgsRaw
opts = info (argParser <**> helper)
      ( fullDesc
      <> progDesc "Heuristics for rearrangement distance with replicated genes.")

getArgs :: (MonadIO mon) => mon ArgsRaw
getArgs = liftIO $ execParser opts

processMH :: MapHeur -> MapHeur
processMH (A3 genalg@(GenAlg _ keep lom him lox hix _ _ gm gc)) = A3 $ genalg
  { cg_gm = if gm == -1 then keep else gm
  , cg_gc = if gc == -1 then keep `div` 2 else gc
  , cg_him = if him == -1 then lom else him
  , cg_hix = if hix == -1 then lox else hix }
processMH (CX sm@(SepMeta _ _ (A3 genalg@(GenAlg _ keep lom him lox hix _ _ gm gc)))) = CX $ sm
  { sm_mh = A3 $ genalg
    { cg_gm = if gm == -1 then keep else gm
    , cg_gc = if gc == -1 then keep `div` 2 else gc
    , cg_him = if him == -1 then lom else him
    , cg_hix = if hix == -1 then lox else hix }}
processMH (A15 memalg@(MemAlg _ keep lox hix _ gc _ _)) = A15 $ memalg
  { cm_gc = if gc == -1 then keep `div` 2 else gc
  , cm_hix = if hix == -1 then lox else hix }
processMH mh = mh

processArgs :: ArgsRaw -> Args
processArgs raw_args = validate processed
  where
  com = case raw_com raw_args of
          MH mh -> MH $ processMH mh
          _ -> raw_com raw_args
  isSign = if raw_isSign raw_args then Signed else Unsigned
  reMod = raw_reMod raw_args
  total = raw_createTotal raw_args
  onlyDup = raw_onlyDup raw_args
  withMap = raw_withMap raw_args
  contNei = raw_contNei raw_args
  useAss = raw_useAss raw_args
  mcspCom = raw_mcspCom raw_args
  processed = Args
    { distArgs = (isSign, reMod, com, total, onlyDup, withMap, contNei, useAss, mcspCom)
    , modelArg = reMod
    , permId = raw_permId raw_args
    , permProg = raw_permProg raw_args
    , getMin = raw_getMin raw_args
    , input = raw_input raw_args
    , output = raw_output raw_args
    , noParallel = raw_noPar raw_args
    , singleLine = raw_singleLine raw_args
    }
  validate args
    | reMod == Trans && isSign == Signed = throw $ OtherError "Transposition model not applicable to signed strings" "processArgs"
    | permId args == 0 && permProg args == "" = throw $ OtherError "No program selected to use on permutations" "processArgs"
    | otherwise = processComands
      where
      processComands = case com of
        MH (A4 (Local initial each))
          | initial > total -> throw $ InvalidArgument "initial > total (Local Search)" "processArgs"
          | each > total -> throw $ InvalidArgument "each > total (Local Search)" "processArgs"
          | otherwise -> args
        MH (A7 (Tabu each _))
          | each > total -> throw $ InvalidArgument "each > total (Tabu Search)" "processArgs"
          | otherwise -> args
        MH (A14 (Cuckoo initial _ _))
          | initial > total -> throw $ InvalidArgument "initial > total (Cuckoo Search)" "processArgs"
          | otherwise -> args
        MH (A5 (Grasp initial create_rlc create_loc _ _))
          | initial > total -> throw $ InvalidArgument "initial > total (GRASP)" "processArgs"
          | create_rlc > total -> throw $ InvalidArgument "create on rlc > total (GRASP)" "processArgs"
          | create_loc > total -> throw $ InvalidArgument "create on Local Search > total (GRASP)" "processArgs"
          | otherwise -> args
        MH (A3 (GenAlg initial keep lom him lox hix _ _ gm gc))
          | initial > total -> throw $ InvalidArgument "initial > total (GenAlg)" "processArgs"
          | lom > him && him /= -1 -> throw $ InvalidArgument "lower value of Mutation > highest value of Mutation (GenAlg)" "processArgs"
          | lox > hix && hix /= -1 -> throw $ InvalidArgument "lower value of Crossover > highest value of Crossover (GenAlg)" "processArgs"
          | gc > keep -> throw $ InvalidArgument "generate by Crossover > keep (GenAlg)" "processArgs"
          | gm > keep -> throw $ InvalidArgument "generate by Mutation > keep (GenAlg)" "processArgs"
          | otherwise -> args
        MH (A8 (SimAnn max_iter temp0 k a))
          | k <= 0.0 -> throw $ InvalidArgument "Non positive multiplicative constant (SimAnn)" "processArgs"
          | a <= 0 || a >= 100 -> throw $ InvalidArgument "Change rate outside interval (0,100) (SimAnn)" "processArgs"
          | max_iter <= 0 -> throw $ InvalidArgument "Non positive number of iterations (SimAnn)" "processArgs"
          | temp0 <= 0 -> throw $ InvalidArgument "Non positive initial temperature (SimAnn)" "processArgs"
          | otherwise -> args
        MH (A15 (MemAlg initial keep lox hix _ gc create_loc _))
          | initial > total -> throw $ InvalidArgument "initial > total (MemAlg)" "processArgs"
          | lox > hix && hix /= -1 -> throw $ InvalidArgument "lower value of Crossover > highest value of Crossover (MemAlg)" "processArgs"
          | gc > keep -> throw $ InvalidArgument "generate by Crossover > keep (MemAlg)" "processArgs"
          | create_loc > total -> throw $ InvalidArgument "create on Local Search > total (MemAlg)" "processArgs"
          | otherwise -> args
        MH (A9 _)
          | contNei -> throw $ InvalidArgument "cannot use contracted neighbors (Sep)" "processArgs"
          | not onlyDup -> throw $ InvalidArgument "heuristic only for duplicated characters (Sep)" "processArgs"
          | otherwise -> args
        MH (CX (SepMeta initial _ _))
          | initial > total -> throw $ InvalidArgument "initial > total (SepMeta)" "processArgs"
          | contNei -> throw $ InvalidArgument "cannot use contracted neighbors (SepMeta)" "processArgs"
          | not onlyDup -> throw $ InvalidArgument "heuristic only for duplicated characters (SepMeta)" "processArgs"
          | otherwise -> args
        SD (A6 _)
          | useAss -> throw $ InvalidArgument "Soar already applies the assignments (Soar)" "processArgs"
          | otherwise -> args
        _ -> args
