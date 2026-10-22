{-# LANGUAGE FlexibleContexts #-}

module ArgsDB where

------------------------------------------------------
import Types
------------------------------------------------------
import Options.Applicative (Parser, ParserInfo, option, auto, long, short, metavar, help, value, showDefault, switch, subparser, command, info, (<**>), helper, progDesc, strOption, fullDesc, execParser)
import Control.Monad.IO.Class (MonadIO, liftIO)
------------------------------------------------------

opts :: ParserInfo Args
opts = info (argParser <**> helper)
      ( fullDesc
      <> progDesc "Generate data base with genomes. Used for tests of solutions for rearrangement problems.")

data DBType = NormalDB | HardDB deriving (Read, Show, Eq)

data Parameters = DB1 DupDB | DB2 RepDB | DB3 RandDB

newtype DupDB = DupDB { db_dup :: Dup }

data RepDB = RepDB {
    db_low :: Int
  , db_high :: Int
  , db_rep :: Rep
  }

newtype RandDB = RandDB { db_lim :: Int }

data Args = Args {
    db_par :: Parameters
  , db_repetitions :: Int
  , db_size :: Size
  , db_nop :: Int
  , db_porc :: Int
  , db_sign :: Bool 
  , db_type :: DBType
  , db_output :: String
  }

dupDBParser :: Parser DupDB
dupDBParser = DupDB
      <$> option auto
          ( long "duplicated"
         <> short 'd'
         <> metavar "D"
         <> help "Number of duplicated genes." )

repDBParser :: Parser RepDB
repDBParser = RepDB
      <$> option auto
          ( long "low"
         <> short 'l'
         <> metavar "L"
         <> help "Minimum number of replicas." )
      <*> option auto
          ( long "high"
         <> short 'h'
         <> metavar "H"
         <> help "Maximum number of replicas." )
      <*> option auto
          ( long "multiplicated"
         <> short 'd'
         <> metavar "D"
         <> help "Number of multiplicated genes." )

randDBParser :: Parser RandDB
randDBParser = RandDB
      <$> option auto
          ( long "lim"
         <> short 'l'
         <> metavar "L"
         <> help "Maximum value for a gene." )

argParser :: Parser Args
argParser = Args
      <$> subparser
          ( command "DupDB" (info
                           (DB1 <$> dupDBParser <**> helper)
                           (progDesc "DB only with duplicated genes."))
          <> command "RepDB" (info
                           (DB2 <$> repDBParser <**> helper)
                           (progDesc "DB with replicated genes."))
          <> command "RandDB" (info
                           (DB3 <$> randDBParser <**> helper)
                           (progDesc "DB with random target genomes.")))
      <*> option auto
          ( long "number_genomes"
         <> short 'k'
         <> metavar "K"
         <> help "Number genome pairs to generate.")
      <*> option auto
          ( long "size_genome"
         <> short 'n'
         <> metavar "N"
         <> help "Size of the genomes." )
      <*> option auto
          ( long "number_op"
         <> short 'r'
         <> metavar "R"
         <> help "Number of operations to apply (-1 to use a random list)." )
      <*> option auto
          ( long "porcentage_rev"
         <> short 'p'
         <> metavar "P"
         <> showDefault
         <> value 100
         <> help "Porcentage of reversions in the operations." )
      <*> switch
          ( long "sign"
         <> short 's'
         <> help "Consider the Genomes sign.")
      <*> option auto
          ( long "genome-type"
         <> short 't'
         <> help "An option to generate a harder version of the DB.")
      <*> strOption
          ( long "outfile"
         <> short 'o'
         <> metavar "oFILE"
         <> help "Output file")

getArgs :: (MonadIO mon) => mon Args
getArgs = liftIO $ execParser opts
