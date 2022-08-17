module DatabaseExporter.CommandLineParser where

import Options.Applicative
import Data.Functor.Identity
import Data.Word

data Config =
   Export
    { dbPath     :: FilePath
    , exportPath :: FilePath
    , chunkSize  :: Word64
    }
  | Check
    { file :: FilePath }

config :: Parser (Identity Config)
config =
  Identity <$>
  (hsubparser
   (metavar "command" <>
    (command
     "export"
      (info
        (Export
         <$> strOption
          (long "dbpath"
            <> metavar "PATH"
            <> help "Database root path")
          <*> strOption
          (long "exportpath"
            <> metavar "PATH"
            <> help "Export path")
          <*> option auto
          (long "chunksize"
            <> metavar "NUM"
            <> help "Maximum number of blocks to export in a single file"))
        (progDesc "Export a database"))) <>
    (command
      "check"
      (info
        (Check
         <$> strOption
         (long "exportpath"
           <> metavar "PATH"
           <> help "Export path"))
        (progDesc "Check an exported database")))))
