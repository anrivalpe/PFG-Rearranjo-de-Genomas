{-# LANGUAGE FlexibleContexts #-}

module ArgsMCSP where

------------------------------------------------------
import Types
------------------------------------------------------
import Options.Applicative (Parser, ParserInfo, option, auto, long, short, metavar, help, value, showDefault, switch, subparser, command, info, (<**>), helper, progDesc, strOption, fullDesc, execParser, commandGroup)
import Control.Monad.IO.Class (MonadIO, liftIO)
------------------------------------------------------

opts :: ParserInfo ArgsRaw
opts = info (argParser <**> helper)
      ( fullDesc
      <> progDesc "Simplify the Genomes with a Minimum Common Partition (MCSP) algorithm.")

data Commands = P0 ID | P1 Soar | P2 HS | P3 Combine | P4 Greedy

data Variation = Direct | Reverse | Signed deriving (Show, Read)

data ID = ID
data Soar = Soar
    { so_aprox :: Aprox
    , so_combine :: Bool
    } deriving (Eq)
data HS = HS
    { hs_combi :: Bool
    } deriving (Eq)
data Combine = Combine
data Greedy = Greedy
    { gr_withSin :: Bool
    } deriving (Eq)

data ArgsRaw = ArgsRaw
  { raw_variation :: Variation
  , raw_input :: String
  , raw_output :: String
  , raw_onlyDup :: Bool
  , raw_useAss :: Bool
  , raw_noPar :: Bool
  , raw_singleLine :: Bool
  , raw_com :: Commands
  }

type MCSPArgs = (Variation, Commands, Bool, Bool)
data Args = Args {
    mcspArgs :: MCSPArgs
  , input :: String
  , output :: String
  , noParallel :: Bool
  , singleLine :: Bool
  }

idParser :: Parser ID
idParser = pure ID

soarParser :: Parser Soar
soarParser = Soar
      <$> option auto
          ( long "approximation"
         <> short 'a'
         <> metavar "AP"
         <> value Ap4
         <> help "Aproximation algoritm to use." )
      <*> switch
          ( long "use-combine"
         <> help "Use combination heuristic." )

hsParser :: Parser HS
hsParser = HS
      <$> switch
          ( long "use-combine"
         <> help "Use combination heuristic." )

combineParser :: Parser Combine
combineParser = pure Combine

greedyParser :: Parser Greedy
greedyParser = Greedy
      <$> switch
          ( long "with-sin"
         <> help "Prefer blocks with singletons (Novel)." )

mcspParser :: Parser Commands
mcspParser =  subparser $
          commandGroup "MCSP_ALGO: Algorithm to use for MCSP"
          <> metavar "MCSP_ALGO"
          <> command "ID" (info
                           (P0 <$> idParser <**> helper)
                           (progDesc "No partition."))
          <> command "Soar" (info
                           (P1 <$> soarParser <**> helper)
                           (progDesc "Partition from SOAR."))
          <> command "HS" (info
                           (P2 <$> hsParser <**> helper)
                           (progDesc "Partition from HS (Theta(k) approximation)."))
          <> command "Combine" (info
                           (P3 <$> combineParser <**> helper)
                           (progDesc "Use Combine algoritm."))
          <> command "Greedy" (info
                           (P4 <$> greedyParser <**> helper)
                           (progDesc "Use Greedy algoritm."))

argParser :: Parser ArgsRaw
argParser = ArgsRaw
      <$> option auto
          ( long "variation"
         <> short 'v'
         <> metavar "VAR"
         <> showDefault
         <> value Direct
         <> help "Variation of the problem to consider. One of Direct, Reverse or Signed.")
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
          ( long "ass"
         <> short 'a'
         <> help "Use the initial assignments. May improve the result if many replicated genes are present." )
      <*> switch
          ( long "no-par"
         <> help "Do not process the genomes in parallel.")
      <*> switch
          ( long "single-line"
         <> help "Target genome is the identity.")
      <*> mcspParser

getArgs :: (MonadIO mon) => mon ArgsRaw
getArgs = liftIO $ execParser opts

processArgs :: ArgsRaw -> Args
processArgs raw_args = processed
  where
  com = raw_com raw_args
  variation_ = raw_variation raw_args
  onlyDup = raw_onlyDup raw_args
  useAss = raw_useAss raw_args
  processed = Args
    { mcspArgs = (variation_, com, onlyDup, useAss)
    , input = raw_input raw_args
    , output = raw_output raw_args
    , noParallel = raw_noPar raw_args
    , singleLine = raw_singleLine raw_args
    }
