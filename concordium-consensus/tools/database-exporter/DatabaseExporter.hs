{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE BangPatterns               #-}
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
  exportDatabaseV1,
  -- * Exporting
  initialReadingHandle,
  readExportedDatabaseV1
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
import           Data.ByteString as BS hiding (putStrLn, tail, last, sort)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Serialize as S
import           Data.Time.Clock
import           Lens.Micro.Platform
import           System.Directory
import           System.FilePath.Posix
import           System.IO
import           Concordium.GlobalState.TreeState (ImportingResult(..), readBlocksV1)

data ReadEnv = ReadEnv
    { _db         :: DatabaseHandlers (BlockStatePointer PersistentBlockState)
    , _exportFile :: Handle
    }
makeLenses ''ReadEnv

initialDatabase :: FilePath -> IO (DatabaseHandlers (BlockStatePointer PersistentBlockState))
initialDatabase p = makeDatabaseHandlers p True

initialHandle :: FilePath -> IO Handle
initialHandle p = do
  exists <- doesFileExist p
  if exists then do
    removeFile p
    openBinaryFile p WriteMode
  else do
    createDirectoryIfMissing True (takeDirectory p)
    openBinaryFile p WriteMode

exportHeaderV1 :: ReaderT ReadEnv IO ()
exportHeaderV1 = do
  export <- view exportFile
  let header = S.encode (1 :: Version)
  liftIO $ BS.hPut export header

exportBlockV1 :: BlockHeight -> ReaderT ReadEnv IO ()
exportBlockV1 finalizedHeight = do
  theDB <- view db
  mblock <- liftIO $ resizeOnResized theDB (getFinalizedBlockAtHeight theDB finalizedHeight)
  case mblock of
    Nothing -> return ()
    Just storedBlock -> do
      let serializedBlock = S.runPut (putBlockV1 (sbBlock storedBlock))
          len = BS.length serializedBlock
      export <- view exportFile
      liftIO $ do
        putStrLn $ "Exported block: " ++ show (_bpHash (sbInfo storedBlock))
          ++ " at height " ++ show finalizedHeight
          ++ " with blockSlot " ++ show (blockSlot (sbBlock storedBlock))
          ++ ". Serialized length: " ++ show (8 + len)
        BS.hPut export (S.runPut (S.putWord64be (fromIntegral len))) -- put length
        BS.hPut export serializedBlock -- put block bytes
      exportBlockV1 (finalizedHeight + 1)

-- |Export the database in V1 format. This currently puts the version number first (1), and after that
-- the list of blocks in format (length, block bytes) where length is serialized in big-endian Word64.
exportDatabaseV1 :: ReaderT ReadEnv IO ()
exportDatabaseV1 = do
  exportHeaderV1
  exportBlockV1 1
  export <- view exportFile
  liftIO $ do
    hClose export
    putStrLn "Done."


initialReadingHandle :: FilePath -> IO Handle
initialReadingHandle p = openBinaryFile p ReadMode

readExportedDatabaseV1 :: Handle -> IO ()
readExportedDatabaseV1 h = do
  liftIO $ do
    tm <- liftIO getCurrentTime
    lbs <- LBS.hGetContents h -- file is automatically closed after EOF is reached
    putStrLn "Starting database processing"
    result <- readBlocksV1 lbs tm (\_ src str-> putStrLn $ show src ++ ": " ++ str) GlobalState (const $ return Success) :: IO (ImportingResult ())
    case result of
      SerializationFail -> putStrLn "Error."
      _ -> putStrLn "Done."
