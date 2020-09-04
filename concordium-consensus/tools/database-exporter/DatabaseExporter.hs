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
import           Concordium.GlobalState.TreeState (ImportingResult(..), readBlocks)

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


readExportedDatabase :: DatabaseExporterMonad ()
readExportedDatabase = do
  export <- view exportFile
  liftIO $ do
    tm <- liftIO getCurrentTime
    lbs <- LBS.hGetContents export -- file is automatically closed after EOF is reached
    putStrLn "Starting database processing"
    result <- readBlocks lbs tm (\_ _ -> putStrLn) undefined (const $ return Success) :: IO (ImportingResult ())
    case result of
      SerializationFail -> putStrLn "Error."
      _ -> putStrLn "Done."
