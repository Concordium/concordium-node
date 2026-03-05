{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | This tool provides functionality for exporting a node database for use with the out-of-band
--  catch up mechanism.  It also provides functionality for checking that such an exported set of
--  blocks is correctly serialized.
module Main where

import Concordium.Logger
import Concordium.Types
import Data.Functor.Identity
import qualified Options.Applicative as Options

import qualified BlockStateDump.Config as Config
import qualified BlockStateDump.DumpState as DumpState

-- | Dump part of blobstore specified in command
main :: IO ()
main = do
    Identity conf <- Options.execParser opts
    let commandAction = case conf of
            Config.DumpState{..} ->
                case promoteProtocolVersion cProtocolVersion of
                    SomeProtocolVersion spv ->
                        DumpState.dumpState spv cTreeStateDbPath cAccountMapDbPath cBlockStatePath cOutDir cStartBlockHeight cEndBlockHeight
    runLoggerT commandAction logm
  where
    opts =
        Options.info
            (Config.config Options.<**> Options.helper)
            ( Options.fullDesc
                <> Options.progDesc "Dump blobstore content"
            )
    logm _ lvl s = putStrLn $ show lvl ++ ": " ++ s
