{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | This tool provides functionality for exporting a node database for use with the out-of-band
--  catch up mechanism.  It also provides functionality for checking that such an exported set of
--  blocks is correctly serialized.
module Main where

import Control.Monad
import Data.Functor.Identity
import qualified Data.Serialize as S
import Data.Time
import qualified Options.Applicative as Options
import System.Exit

import Concordium.GlobalState.Block
import Concordium.GlobalState.Finalization
import Concordium.Logger
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Types.Parameters

import qualified BlobstoreDump.CommandLineParser as CommandLineParser

-- | Dump block state from the node database.
dumpState ::
    -- Path to node database, e.g. xyz/database-v4.
    FilePath ->
    IO ()
dumpState filepath = do
    logm External LLInfo $ "Dumping block state from database: " ++ filepath
  where
    logm _ lvl s = putStrLn $ show lvl ++ ": " ++ s

-- | Dump part of blobstore specified in command
main :: IO ()
main = do
    Identity conf <- Options.execParser opts
    case conf of
        CommandLineParser.DumpState file -> dumpState file
  where
    opts =
        Options.info
            (CommandLineParser.config Options.<**> Options.helper)
            ( Options.fullDesc
                <> Options.progDesc "Dump blobstore content"
            )
