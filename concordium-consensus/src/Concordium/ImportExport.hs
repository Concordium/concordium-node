{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- |Functionality for importing and exporting the block database.
--
-- The block database format is as follows:
--
-- * Version header (Version - variable length): 3
-- * One or more sections
--
-- Each section consists of:
--
-- * The length of the section header [not including this length] (Word64be)
-- * The length of the section including the header, length, etc. (Word64be)
-- * The genesis index of blocks in the section (Word32be)
-- * The protocol version of this section (Word64be)
-- * The genesis block hash (32 bytes)
-- * The block height of the first block (Word64be)
-- * The number of blocks present (Word64be)
-- * The total length of the block portion (Word64be)
-- * The number of finalization records (Word64be)
-- * The block portion, consisting for each block of:
--    - The length of the serialized block (Word64be)
--    - The serialized, versioned block data
-- * The finalization record portion, consisting for each finalization record of:
--    - The length of the finalization record (Word64be)
--    - The serialized, versioned finalization record
--
-- Within a section, the blocks must be sequential and of the correct number.
-- The finalization records must also be sequential and of the correct number.
-- The finalization records must finalize blocks that are included in the section
-- for which there is not a finalization record included in another block.
--
-- Sections themselves should be ordered with sequential genesis indexes.
--
-- It is expected that each section should contain all finalized blocks except the
-- genesis block, and all finalization records that are not already included in blocks.
module Concordium.ImportExport where

import Control.Monad
import Control.Monad.Trans.Except
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe
import Data.Serialize
import Data.Word
import System.Directory
import System.FilePath
import System.IO

import Concordium.Common.Version
import Concordium.GlobalState.Block
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Persistent.LMDB
import Concordium.Logger
import Concordium.Types
import Concordium.Utils.Serialization.Put
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.State (MonadState, evalStateT)
import Data.Bits
import Lens.Micro.Platform

-- |State used for exporting the database.
newtype DBState pv = DBState
    { _dbsHandlers :: DatabaseHandlers pv ()
    }

makeLenses ''DBState

instance HasDatabaseHandlers pv () (DBState pv) where
    dbHandlers = dbsHandlers

-- |A section header of an exported block database
data SectionHeader = SectionHeader
    { sectionLength :: !Word64,
      sectionGenesisIndex :: !GenesisIndex,
      sectionProtocolVersion :: !ProtocolVersion,
      sectionGenesisHash :: !BlockHash,
      sectionFirstBlockHeight :: !BlockHeight,
      sectionBlockCount :: !Word64,
      sectionBlocksLength :: !Word64,
      sectionFinalizationCount :: !Word64
    }

instance Serialize SectionHeader where
    put SectionHeader{..} = do
        putWord64be sectionLength
        put sectionGenesisIndex
        put sectionProtocolVersion
        put sectionGenesisHash
        put sectionFirstBlockHeight
        putWord64be sectionBlockCount
        putWord64be sectionBlocksLength
        putWord64be sectionFinalizationCount
    get = do
        sectionLength <- getWord64be
        sectionGenesisIndex <- get
        sectionProtocolVersion <- get
        sectionGenesisHash <- get
        sectionFirstBlockHeight <- get
        sectionBlockCount <- getWord64be
        sectionBlocksLength <- getWord64be
        sectionFinalizationCount <- getWord64be
        return SectionHeader{..}

-- |A dummy 'SectionHeader' that is used as a placeholder when writing a section, before being
-- overwritten with the correct data.
placeholderSectionHeader :: SectionHeader
placeholderSectionHeader = SectionHeader 0 0 P1 (BlockHash minBound) 0 0 0 0

-- |The length of a section header in bytes.
sectionHeaderLength :: Word64
sectionHeaderLength = fromIntegral $ BS.length $ encode placeholderSectionHeader

-- |Open a file handle for writing a chunk.
initialHandle :: FilePath -> IO Handle
initialHandle p = do
    createDirectoryIfMissing True (takeDirectory p)
    openBinaryFile p WriteMode

-- |Export a database in V3 format, as a collection of block file chunks, given the data directory
-- root and the export directory root.
exportDatabaseV3 ::
    -- |Data directory
    FilePath ->
    -- |Export directory
    FilePath ->
    -- |Chunk size
    Word64 ->
    IO ()
exportDatabaseV3 dbDir outDir chunkSize = do
  createDirectoryIfMissing True outDir
  bracket (openFile (outDir </> "blocks.idx") AppendMode) hClose
    (\indexHdl -> exportSections dbDir outDir indexHdl chunkSize 0)

-- |Export a database section for each genesis index.
exportSections ::
    -- |Database directory
    FilePath ->
    -- |Export directory
    FilePath ->
    -- |Index file handle
    Handle ->
    -- |Chunk size
    Word64 ->
    -- |Next genesis index to export
    GenesisIndex ->
    IO ()
exportSections dbDir outDir indexHdl chunkSize genIndex = do
    let treeStateDir = dbDir </> "treestate-" ++ show genIndex
    -- Check if the database exists for this genesis index
    dbEx <- doesPathExist $ treeStateDir </> "data.mdb"
    if dbEx
        then
            openReadOnlyDatabase treeStateDir >>= \case
                Nothing -> putStrLn "Tree state database could not be opened."
                Just (VersionDatabaseHandlers (dbh :: DatabaseHandlers pv ())) -> do
                    evalStateT
                        ( do
                            mgenFinRec <- resizeOnResized $ readFinalizationRecord 0
                            forM_ mgenFinRec $ \genFinRec -> do
                                let genHash = finalizationBlockPointer genFinRec
                                    startHeight = 1
                                writeChunks
                                    genIndex
                                    (demoteProtocolVersion (protocolVersion @pv))
                                    genHash
                                    startHeight
                                    outDir
                                    indexHdl
                                    chunkSize
                        )
                        (DBState dbh)
                    closeDatabase dbh
                    exportSections dbDir outDir indexHdl chunkSize (genIndex + 1)
        else putStrLn ("The tree state database does not exist at " ++ treeStateDir)

-- |Write a database section as a collection of chunks in the specified directory. The exported
-- chunks will be accompanied by an index file mapping chunk filenames to the block ranges stored in
-- them. Each line in index file has format FILENAME,FIRSTBLOCK,LASTBLOCK\n
writeChunks ::
    (IsProtocolVersion pv, MonadIO m, MonadState (DBState pv) m, MonadCatch m) =>
    -- |Genesis index
    GenesisIndex ->
    -- |Protocol version
    ProtocolVersion ->
    -- |Genesis block hash
    BlockHash ->
    -- |Height of first block in section
    BlockHeight ->
    -- |Export directory
    FilePath ->
    -- |Index file handle
    Handle ->
    -- |Chunk size
    Word64 ->
    m ()
writeChunks
    sectionGenesisIndex
    sectionProtocolVersion
    sectionGenesisHash
    sectionFirstBlockHeight
    outDir indexHdl chunkSize = do
        let chunkName = outDir </> "blocks-"
                        ++ show sectionGenesisIndex ++ "-"
                        ++ show sectionFirstBlockHeight
                        ++ ".dat"
        chunkHdl <- liftIO $ initialHandle chunkName

        (sectionStart, blocksStart) <- liftIO $ do
            BS.hPut chunkHdl $ encode (3 :: Version)
            sectionStart <- hTell chunkHdl
            -- Write a dummy section header that we will later overwrite
            runPutH (liftPut $ putWord64be sectionHeaderLength >> put placeholderSectionHeader) chunkHdl
            blocksStart <- hTell chunkHdl
            return (sectionStart, blocksStart)
        -- how many blocks would it take to fill the exported chunk to its `chunkSize`
        -- (non-zero means we're out of blocks to export in the current section)
        (chunkUnused, finRecsMissing) <- exportBlocksToChunk chunkHdl sectionFirstBlockHeight chunkSize
        let sectionBlockCount = chunkSize - chunkUnused
        blocksEnd <- liftIO $ hTell chunkHdl
        sectionFinalizationCount <- exportFinRecsForChunk chunkHdl finRecsMissing
        liftIO $ do
            sectionEnd <- hTell chunkHdl
            -- Go back to the start and rewrite the section header with the correct data
            hSeek chunkHdl AbsoluteSeek sectionStart
            let sectionHeader =
                    SectionHeader
                        { sectionLength = fromInteger (sectionEnd - sectionStart),
                          sectionBlocksLength = fromInteger (blocksEnd - blocksStart),
                          ..
                        }
            runPutH (liftPut $ putWord64be sectionHeaderLength >> put sectionHeader) chunkHdl
            hClose chunkHdl
            TIO.hPutStrLn indexHdl . T.pack $
              takeFileName chunkName
              ++ "," ++ show sectionFirstBlockHeight
              ++ "," ++ show (sectionFirstBlockHeight + BlockHeight sectionBlockCount - 1)
            putStrLn $
              "Exported chunk " ++ takeFileName chunkName
              ++ " containing blocks with heights from " ++ show sectionFirstBlockHeight
              ++ " to " ++ show (sectionFirstBlockHeight + BlockHeight sectionBlockCount - 1)
              ++ " and " ++ show sectionFinalizationCount ++ " finalization records"
        if chunkUnused == 0
          then writeChunks
                   sectionGenesisIndex
                   sectionProtocolVersion
                   sectionGenesisHash
                   (sectionFirstBlockHeight + BlockHeight chunkSize)
                   outDir
                   indexHdl
                   chunkSize
          else return ()

-- |Export a series of blocks as a chunk of a specified length.
exportBlocksToChunk ::
  forall pv m.
    (IsProtocolVersion pv, MonadIO m, MonadState (DBState pv) m, MonadCatch m) =>
    -- |Handle to export to
    Handle ->
    -- |Height of next block to export
    BlockHeight ->
    -- |Number of blocks to export
    Word64 ->
    -- |(Block counter final value, finalization records not included in blocks)
    m (Word64, [FinalizationIndex])
exportBlocksToChunk hdl height count =
  resizeOnResized (readFinalizedBlockAtHeight height) >>= \case
            Nothing -> return (count, [])
            Just sb | NormalBlock normalBlock <- sbBlock sb -> do
                let serializedBlock =
                        runPut $ putVersionedBlock (protocolVersion @pv) normalBlock
                    len = fromIntegral $ BS.length serializedBlock
                liftIO $ do
                    BS.hPut hdl $ runPut $ putWord64be len
                    BS.hPut hdl serializedBlock
                let finRecIndex :: Maybe FinalizationIndex = join . forM (blockFields (sbBlock sb)) $ \bf ->
                      case blockFinalizationData bf of
                        BlockFinalizationData _ -> Nothing
                        _ -> Just (sbFinalizationIndex sb)
                if count > 1
                  then (_2 %~ (maybeToList finRecIndex ++)) <$> exportBlocksToChunk hdl (height + 1) (count - 1)
                  else return (0, maybeToList finRecIndex)
            Just _ -> return (count, []) -- this branch should never be reachable, genesis blocks are not exported.

-- |Export finalization records with specified finalization indexes
exportFinRecsForChunk ::
  forall pv m.
    (MonadIO m, MonadState (DBState pv) m, MonadCatch m) =>
    -- |Handle to export to
    Handle ->
    -- |Finalization records to export
    [FinalizationIndex] ->
    -- |Number of exported finalization records
    m Word64
exportFinRecsForChunk hdl finRecIndexes = do
  result <- forM finRecIndexes $ \finRecIndex ->
    resizeOnResized (readFinalizationRecord finRecIndex) >>= \case
      Nothing -> return 0
      Just fr -> do
        let serializedFr = runPut $ putVersionedFinalizationRecordV0 fr
            len = fromIntegral $ BS.length serializedFr
        liftIO $ do
          BS.hPut hdl $ runPut $ putWord64be len
          BS.hPut hdl serializedFr
        return 1
  return (sum result)

-- |Imported data for processing.
data ImportData
    = ImportBlock ProtocolVersion GenesisIndex BS.ByteString
    | ImportFinalizationRecord ProtocolVersion GenesisIndex BS.ByteString

-- |Failure result of importing data.
data ImportFailure a
    = ImportSerializationFail
    | ImportOtherError a

-- |Alias for the result of importing data.
type ImportResult a b = Either (ImportFailure a) b

-- |Get bytes representing a version number.
getVersionBytes :: Handle -> IO BS.ByteString
getVersionBytes h = do
    b <- BS.hGet h 1
    if testBit (BS.head b) 7
        then BS.append b <$> getVersionBytes h
        else return b

-- |Import blocks and finalization records from an exported block file.
importBlocksV3 ::
    forall m a.
    (MonadIO m, MonadLogger m, MonadMask m) =>
    -- |File to import from
    FilePath ->
    -- |First genesis index to import data from
    GenesisIndex ->
    -- |Callback to import data
    (ImportData -> m (ImportResult a ())) ->
    m (ImportResult a ())
importBlocksV3 inFile firstGenIndex cbk = runExceptT $
    handle onIOErr $
        bracket (liftIO $ openBinaryFile inFile ReadMode) (liftIO . hClose) $ \hdl -> do
            v <- liftIO $ getVersionBytes hdl
            case decode v of
                Left err -> failWith $ "Error deserializing version header: " ++ err
                Right version
                    | version == supportedVersion -> importSections hdl
                    | otherwise ->
                        failWith $
                            "Block file version is " ++ show version
                                ++ " which is not supported. Only version "
                                ++ show supportedVersion
                                ++ " is supported."
  where
    supportedVersion :: Version
    supportedVersion = 3
    failWith :: String -> ExceptT (ImportFailure a) m r
    failWith s = do
        logEvent External LLError $ "Error importing blocks: " ++ s
        throwE ImportSerializationFail
    -- We handle all IO errors as serialization failures from a result perspective.
    onIOErr :: IOError -> ExceptT (ImportFailure a) m ()
    onIOErr = failWith . show
    importSections hdl = do
        eof <- liftIO $ hIsEOF hdl
        unless eof $ do
            sectionStart <- liftIO $ hTell hdl
            sectionBS <- getLengthByteString hdl
            case decode sectionBS of
                Left err -> failWith err
                Right SectionHeader{..} -> do
                    when (sectionGenesisIndex >= firstGenIndex) $ do
                        replicateM_
                            (fromIntegral sectionBlockCount)
                            ( importData hdl $
                                ImportBlock sectionProtocolVersion sectionGenesisIndex
                            )
                        replicateM_
                            (fromIntegral sectionFinalizationCount)
                            ( importData hdl $
                                ImportFinalizationRecord sectionProtocolVersion sectionGenesisIndex
                            )
                    -- Move to the next section
                    liftIO $ hSeek hdl AbsoluteSeek (sectionStart + toInteger sectionLength)
                    importSections hdl
    importData :: Handle -> (BS.ByteString -> ImportData) -> ExceptT (ImportFailure a) m ()
    importData hdl makeImport = do
        blockBS <- getLengthByteString hdl
        ExceptT $ cbk $ makeImport blockBS
    getLengthByteString :: Handle -> ExceptT (ImportFailure a) m BS.ByteString
    getLengthByteString hdl = do
        lenBS <- liftIO $ BS.hGet hdl 8
        case fromIntegral <$> runGet getWord64be lenBS of
            Left _ -> failWith "unexpected end of file"
            Right len -> do
                bs <- liftIO $ BS.hGet hdl len
                if BS.length bs == len
                    then return bs
                    else failWith "unexpected end of file"
