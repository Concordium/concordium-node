module BlockStateDump.Config where

import Data.Functor.Identity
import Options.Applicative

import Concordium.Types

data Config
    = DumpState
    { cTreeStateDbPath :: FilePath,
      cAccountMapDbPath :: FilePath,
      cBlockStatePath :: FilePath,
      cOutDir :: FilePath,
      cProtocolVersion :: ProtocolVersion,
      cStartBlockHeight :: BlockHeight,
      cEndBlockHeight :: BlockHeight
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
                                        ( long "tree-state-db-path"
                                            <> metavar "TREESTATEDBPATH"
                                            <> help "Path to tree state LMDB database directory (e.g. xyz/database-v4/treestate-0)"
                                        )
                                    <*> strOption
                                        ( long "account-map-db-path"
                                            <> metavar "ACCOUNTMAPDBPATH"
                                            <> help "Path to account map LMDB database directory (e.g. xyz/database-v4/accountmap)"
                                        )
                                    <*> strOption
                                        ( long "block-state-path"
                                            <> metavar "BLOCKSTATEPATH"
                                            <> help "Path to block state file (e.g. xyz/database-v4/blockstate-0.dat)"
                                        )
                                    <*> strOption
                                        ( long "out-dir"
                                            <> metavar "OUTDIR"
                                            <> help "Path to directory to output dump to"
                                        )
                                    <*> option
                                        auto
                                        ( long "protocol-version"
                                            <> metavar "PROTOCOLVERSION"
                                            <> help "Protocol version for the tree state and block state, e.g. P9"
                                        )
                                    <*> option
                                        auto
                                        ( long "start-block-height"
                                            <> metavar "STARTBLOCKHEIGHT"
                                            <> help "Relative block height to start block state dump at"
                                        )
                                    <*> option
                                        auto
                                        ( long "end-block-height"
                                            <> metavar "ENDBLOCKHEIGHT"
                                            <> help "Relative block height to end block state dump at"
                                        )
                                )
                                (progDesc "Dump block state")
                            )
                       )
                )
            )
