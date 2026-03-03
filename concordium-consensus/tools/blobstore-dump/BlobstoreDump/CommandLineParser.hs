module BlobstoreDump.CommandLineParser where

import Data.Functor.Identity
import Options.Applicative

data Config
    = DumpState
        { dbPath :: FilePath
        }

config :: Parser (Identity Config)
config =
    Identity
        <$> ( hsubparser
                ( metavar "command"
                    <> ( command
                            "state"
                            ( info
                                ( DumpState
                                    <$> strOption
                                        ( long "dbpath"
                                            <> metavar "DBPATH"
                                            <> help "Path to node database directory (e.g. xyz/database-v4)"
                                        )                                    
                                )
                                (progDesc "Dump block state")
                            )
                       )
                )
            )
