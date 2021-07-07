{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : DatabaseExporter

This module provides the functions used when exporting and reading an exported database.

In order to generate the handlers for the database, it uses the function 'Concordium.GlobalState.Persistent.LMDB'
for creating handlers with the same structure as the ones used by consensus to manage its database.

It uses the function 'Concordium.GlobalState.TreeState.readBlocksV1' to check the exported data.
This function uses a type `ImportingResult` that can signal there was a 'SerializationFail'. If this
is not the case, we can be sure that the exported data is consistent. Note that the logging function
is simply printing to stdout and because of that it leaves the source of the logging as undefined.

The continuation provided just accepts every block that is deserialized as we are not checking for duplicates
or any other inconsistencies in the sense of the tree state.

Particularly this has to be in sync with the current block version as it will be the one that is written in
the database and the one that needs to be exported. Also the structure of the tables has to be in sync
with the one in globalstate though this will probably be automatically in sync as we are using the same
constructor for the handles.
-}
module DatabaseExporter (
  -- * Importing
  ReadEnv(..),
  initialHandle,
  initialDatabase,
  exportDatabaseV2,
  -- * Exporting
  initialReadingHandle,
  readExportedDatabaseV2
  ) where

import           Concordium.Logger
import           Concordium.Common.Version
import           Concordium.GlobalState.Block
import           Concordium.GlobalState.BlockPointer
import           Concordium.GlobalState.Persistent.LMDB
import           Concordium.GlobalState.Persistent.BlockState (PersistentBlockState)
import           Concordium.GlobalState.Types
import           Concordium.Types
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.ByteString as BS hiding (putStrLn, tail, last, sort)
import qualified Data.Serialize as S
import           Data.Time.Clock
import           Lens.Micro.Platform
import           System.Directory
import           System.FilePath.Posix
import           System.IO
import           Concordium.GlobalState.TreeState (ImportingResult(..), readBlocksV2)

data ReadEnv = ReadEnv
    { _db         :: DatabaseHandlers 'P1 (BlockStatePointer (PersistentBlockState 'P1))
    , _exportFile :: Handle
    }
makeLenses ''ReadEnv

initialDatabase :: FilePath -> IO (DatabaseHandlers 'P1 (BlockStatePointer (PersistentBlockState 'P1)))
initialDatabase p = makeDatabaseHandlers p True 0

initialHandle :: FilePath -> IO Handle
initialHandle p = do
  exists <- doesFileExist p
  if exists then do
    removeFile p
    openBinaryFile p WriteMode
  else do
    createDirectoryIfMissing True (takeDirectory p)
    openBinaryFile p WriteMode

exportHeaderV2 :: ReaderT ReadEnv IO ()
exportHeaderV2 = do
  export <- view exportFile
  let header = S.encode (2 :: Version)
  liftIO $ BS.hPut export header

exportBlockV2 :: BlockHeight -> ReaderT ReadEnv IO ()
exportBlockV2 finalizedHeight = do
  theDB <- view db
  mblock <- liftIO $ evalStateT (resizeOnResized (readFinalizedBlockAtHeight finalizedHeight)) theDB
  case mblock of
    Nothing -> return ()
    Just storedBlock -> do
      let serializedBlock = S.runPut (putBlock SP1 (sbBlock storedBlock))
          len = BS.length serializedBlock
      export <- view exportFile
      liftIO $ do
        putStrLn $ "Exported block: " ++ show (_bpHash (sbInfo storedBlock))
          ++ " at height " ++ show finalizedHeight
          ++ " with blockSlot " ++ show (blockSlot (sbBlock storedBlock))
          ++ ". Serialized length: " ++ show (8 + len)
        BS.hPut export (S.runPut (S.putWord64be (fromIntegral len))) -- put length
        BS.hPut export serializedBlock -- put block bytes
      exportBlockV2 (finalizedHeight + 1)

-- |Export the database in V2 format. This currently puts the version number first (2), and after that
-- the list of blocks in format (length, block bytes) where length is serialized in big-endian Word64.
exportDatabaseV2 :: ReaderT ReadEnv IO ()
exportDatabaseV2 = do
  exportHeaderV2
  exportBlockV2 1
  export <- view exportFile
  liftIO $ do
    hClose export
    putStrLn "Done."


initialReadingHandle :: FilePath -> IO Handle
initialReadingHandle p = openBinaryFile p ReadMode

readExportedDatabaseV2 :: Handle -> IO ()
readExportedDatabaseV2 h = do
  liftIO $ do
    tm <- liftIO getCurrentTime
    putStrLn "Starting database processing"
    result <- readBlocksV2 h tm (\_ src str-> putStrLn $ show src ++ ": " ++ str) GlobalState (const $ return Success) :: IO (ImportingResult ())
    case result of
      SerializationFail -> putStrLn "Error."
      _ -> putStrLn "Done."
