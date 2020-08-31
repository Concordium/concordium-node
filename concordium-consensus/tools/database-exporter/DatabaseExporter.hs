{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE BangPatterns               #-}
module DatabaseExporter where

import           Concordium.Common.Version
import           Concordium.GlobalState.Block
import           Concordium.GlobalState.BlockPointer
import           Concordium.GlobalState.Persistent.LMDB
import           Concordium.GlobalState.Persistent.BlockState (PersistentBlockState)
import           Concordium.GlobalState.Types
import           Concordium.Types
import           Concordium.Types.Transactions
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
import           System.Exit

data ReadEnv = ReadEnv
    { _path       :: FilePath
    , _exportPath :: FilePath
    , _db         :: DatabaseHandlers (BlockStatePointer PersistentBlockState)
    , _exportFile :: Handle
    }
makeLenses ''ReadEnv

type DatabaseExporterMonad a = ReaderT ReadEnv IO a

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

exportHeaderV0 :: DatabaseExporterMonad ()
exportHeaderV0 = do
  export <- view exportFile
  let header = S.encode (0 :: Version)
  liftIO $ BS.hPut export header

exportBlockV0 :: BlockHeight -> DatabaseExporterMonad ()
exportBlockV0 finalizedHeight = do
  theDB <- view db
  mblock <- liftIO $ resizeOnResized theDB (getFinalizedBlockAtHeight theDB finalizedHeight)
  case mblock of
    Nothing -> return ()
    Just storedBlock -> do
      let serializedBlock = S.runPut (putBlockV0 (sbBlock storedBlock))
          len = BS.length serializedBlock
      export <- view exportFile
      liftIO $ do
        putStrLn $ "Exported block: " ++ show (_bpHash (sbInfo storedBlock))
          ++ " at height " ++ show finalizedHeight
          ++ " with blockSlot " ++ show (blockSlot (sbBlock storedBlock))
          ++ ". Serialized length: " ++ show (8 + len)
        BS.hPut export (S.runPut (S.putWord64be (fromIntegral len))) -- put length
        BS.hPut export serializedBlock -- put block bytes
      exportBlockV0 (finalizedHeight + 1)

-- |Export the database in V0 format. This currently puts the version number first (0), and after that
-- the list of blocks in format (length, block bytes) where length is serialized in big-endian Word64.
exportDatabaseV0 :: DatabaseExporterMonad ()
exportDatabaseV0 = do
  exportHeaderV0
  exportBlockV0 1
  export <- view exportFile
  liftIO $ do
    hClose export
    putStrLn "Done."

initialReadingHandle :: FilePath -> IO Handle
initialReadingHandle p = openBinaryFile p ReadMode

-- |Read the header of the export file.
-- The header consists of
--
-- - version number that determines the format of all the subsequent blocks in the file.
--
-- The function returns, if successfull, the version, and the unconsumed input.
readHeader :: LBS.ByteString -> Either String (Version, LBS.ByteString)
readHeader = S.runGetLazyState S.get

readExportedDatabase :: DatabaseExporterMonad ()
readExportedDatabase = do
  export <- view exportFile
  liftIO $ do
    tm <- liftIO getCurrentTime
    lbs <- LBS.hGetContents export -- file is automatically closed after EOF is reached
    putStrLn "Starting database processing"
    case readHeader lbs of
      Left err -> die $ "Cannot read export file header: " ++ err
      Right (version, rest)
          | version == 0 -> readBlocksFromFileV0 tm rest
          | otherwise -> die $ "Unsupported export file version " ++ show version
  where readBlocksFromFileV0 tm rest
            | LBS.null rest = putStrLn "Finished reading export file."
            | otherwise =
              case S.runGetLazyState blockGetter rest of
                Left err -> die $ "Error reading block bytes: " ++ err
                Right (blockBS, remainingBS) -> do
                  case S.runGet (getBlockV0 (utcTimeToTransactionTime tm)) blockBS of
                    Left e -> die $ "Error deserializing block: " ++ e
                    Right block -> do
                      putStrLn $ "Properly read block at blockSlot " ++ show (blockSlot block)
                      readBlocksFromFileV0 tm remainingBS

        blockGetter = do
          len <- S.getWord64be
          S.getByteString (fromIntegral len)
