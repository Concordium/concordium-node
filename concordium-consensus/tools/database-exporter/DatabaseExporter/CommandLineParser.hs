module DatabaseExporter.CommandLineParser where

import Options.Applicative
import Data.Functor.Identity

data Config =
   Export
    { dbPath     :: FilePath
    , exportPath :: FilePath
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
            <> help "Database path")
          <*> strOption
          (long "exportpath"
            <> metavar "PATH"
            <> help "Export path"))
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
