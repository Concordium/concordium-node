module DatabaseExporter.CommandLineParser where

import Options.Applicative

data Config = Config
    { dbPath     :: Maybe FilePath
    , exportPath :: FilePath
    , readingMode :: Bool
    }

config :: Parser Config
config = Config
      <$> optional (strOption
                    ( long "dbpath"
                      <> metavar "PATH"
                      <> help "Database path" ))
      <*> strOption
          ( long "exportpath"
         <> metavar "PATH"
         <> help "Export path" )
      <*> switch
          (long "read"
          <> short 'r'
          <> help "Read an exported database to check integrity")
