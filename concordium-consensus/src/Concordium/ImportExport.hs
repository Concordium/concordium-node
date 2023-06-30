{-# LANGUAGE GADTs #-}
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
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.State (evalStateT)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import qualified Data.Attoparsec.Text as AP
import Data.Bits
import qualified Data.ByteString as BS
import Data.Char (isHexDigit)
import Data.Maybe (fromMaybe)
import Data.Sequence (
    Seq (..),
    fromList,
    singleton,
    (><),
 )
import Data.Serialize
import qualified Data.Text as T
import Data.Word
import System.Directory
import System.FilePath
import System.IO

import Concordium.Common.Version
import Concordium.GlobalState.Block
import Concordium.GlobalState.BlockPointer
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Persistent.LMDB
import qualified Concordium.KonsensusV1.TreeState.LowLevel as KonsensusV1
import qualified Concordium.KonsensusV1.TreeState.LowLevel.LMDB as KonsensusV1
import qualified Concordium.KonsensusV1.TreeState.Types as KonsensusV1
import qualified Concordium.KonsensusV1.Types as KonsensusV1
import Concordium.Logger
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Utils.Serialization.Put
import Lens.Micro.Platform

-- |State used for exporting the database of a 'ConsensusV0' database.
-- This type exists because we run the exporting in a context that contains
-- a State over the DBState.
--
-- But for the 'ConsensusV1' database we export it via a @Reader@ context,
-- so this type is only used for 'ConsensusV0'.
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
    deriving (Eq, Show)

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

-- |Open a file handle for writing a chunk. If the file with the specified name already exists, a
-- new name is chosen by putting or incrementing a version number in its extension. It is expected
-- that an unversioned filename only has a single extension.
initialHandle :: FilePath -> IO (FilePath, Handle)
initialHandle p = do
    createDirectoryIfMissing True (takeDirectory p)
    chunkExists <- doesFileExist p
    if chunkExists
        then do
            newExt <- getNewExt
            initialHandle (replaceExtensions p newExt)
        else do
            hdl <- openBinaryFile p WriteMode
            return (p, hdl)
  where
    -- The old, potentially unversioned, file extension.
    -- For instance
    --   `oldExt blocks-2-131073.6.dat == ".6.dat"` and
    --   `oldExt blocks-2-131073.dat == ".dat"`.
    oldExt :: String
    oldExt = takeExtensions p
    -- The old file extension with an added or incremented version
    -- number. For instance
    --   if `p == "file.6.dat"` then `newExt == return ".7.dat"` and
    --   if `p == "file.dat"`, then `newExt == return ".2.dat"`.
    getNewExt :: IO String
    getNewExt =
        -- If the old extension is a single extension, i.e. with no
        -- version number, we add one.
        if oldExt == takeExtension p
            then return $ ".2" ++ oldExt
            else case getVersionNumber (T.pack oldExt) of
                Left err ->
                    fail $
                        "Unable to parse a version number from extension '"
                            <> oldExt
                            <> "': "
                            <> err
                Right v -> return $ "." <> show (v + 1) <> ".dat"
    -- Given a file extension, attempt to get its version number.
    -- Returns @Left@ if a version number is not present and @Right@
    -- otherwise.
    getVersionNumber :: T.Text -> Either String Integer
    getVersionNumber = AP.parseOnly $ do
        _ <- AP.char '.'
        AP.decimal

-- |Data type used to represent a line with chunk information in the block index file.
-- A chunk contains exported data for all blocks of height in the range `blockHeightFirst`
-- to `blockHeightLast` and of genesis index `genesisIndex`. When a chunk is exported, a
-- line with the above information and the filename of the chunk is added to the block
-- index file.
data BlockIndexChunkInfo = BlockIndexChunkInfo
    { filename :: T.Text, -- Name of the chunk file.
      genesisIndex :: GenesisIndex, -- Genesis index of the blocks contained in the chunk.
      blockHeightFirst :: BlockHeight, -- Height of the first block contained in the chunk.
      blockHeightLast :: BlockHeight -- Height of the last block contained in the chunk.
    }
    deriving (Show)

-- |Data type used to represent the contents of a block index file.
-- The block index file contains an index of the exported chunks containing the block and
-- and finalization record data of the exported database. The index consists of a number of
-- sections each comprising a section header line that specifies a genesis blockhash followed
-- by a sequence of lines that each represent a chunk containing blocks with that genesis
-- blockhash. A section with genesis blockhash corresponding to a genesis index `i` follows
-- another section with genesis blockhash corresponding to genesis index `j` iff. `i == j`
-- or `i == j+1`.
type BlockIndex = Seq (BlockHash, Seq BlockIndexChunkInfo)

-- |Parse a blockhash.
parseBlockHash :: AP.Parser BlockHash
parseBlockHash = do
    hash <- AP.count 64 $ AP.satisfy isHexDigit
    return $! BlockHash (read hash)

-- |Parse a line of the block index file containing block genesis hash and indicating the start
-- of a new section.
parseGenesisDataLine :: AP.Parser BlockHash
parseGenesisDataLine = do
    _ <- AP.string (T.pack "# genesis hash ")
    hash <- parseBlockHash
    AP.skip AP.isEndOfLine
    return hash

-- |Parse a line of the block index file containing chunk information of a section.
parseChunkLine :: AP.Parser BlockIndexChunkInfo
parseChunkLine = do
    filename <- AP.takeWhile1 (AP.inClass ".a-zA-Z0-9-")
    _ <- AP.char ','
    genesisIndex <- AP.decimal
    _ <- AP.char ','
    blockHeightStart <- AP.decimal
    _ <- AP.char ','
    blockHeightEnd <- AP.decimal
    AP.skip AP.isEndOfLine
    return $ BlockIndexChunkInfo filename genesisIndex blockHeightStart blockHeightEnd

-- | Parse all sections of a block index file.
parseBlockIndexFile :: AP.Parser BlockIndex
parseBlockIndexFile = do
    list <-
        AP.manyTill
            ( do
                genesisData <- parseGenesisDataLine
                chunks <- AP.many' parseChunkLine
                return (genesisData, fromList chunks)
            )
            AP.endOfInput
    return $ fromList list

-- |Show the contents of a block index file. The resulting string can be
-- parsed back into its corresponding @BlockIndex@ using `parseBlockIndexFile`.
showBlockIndexFile :: BlockIndex -> String
showBlockIndexFile = concatMap showSection
  where
    showSection :: (BlockHash, Seq BlockIndexChunkInfo) -> String
    showSection (gd, chunks) =
        showGenesisDataLine gd
            <> concatMap showChunkLine chunks
    showGenesisDataLine :: BlockHash -> String
    showGenesisDataLine gd =
        "# genesis hash "
            <> show gd
            <> "\n"
    showChunkLine :: BlockIndexChunkInfo -> String
    showChunkLine (BlockIndexChunkInfo{..}) =
        T.unpack filename
            <> ","
            <> show genesisIndex
            <> ","
            <> show blockHeightFirst
            <> ","
            <> show blockHeightLast
            <> "\n"

-- |Normalize the block index.
-- Eliminates redundant sections of a `BlockIndex` by 1) merging consecutive
-- sections that have the same genesis block hash and by 2) removing sections
-- with no chunks. This is useful since older versions of the database exporter
-- may insert empty sections or consecutive sections with the same genesis hash.
-- This is due to `exportSections` returning a "tail" of a block index
-- corresponding to the sections it exported. The last section of the previously
-- exported block index will therefore match that of the first section of the tail,
-- so appending the tail to produce the updated block index will introduce some
-- undesired clutter and redundancy.
normalizeBlockIndex :: BlockIndex -> BlockIndex
normalizeBlockIndex ((_, Empty) :<| l) = normalizeBlockIndex l
normalizeBlockIndex ((gh1, chunks1) :<| (gh2, chunks2) :<| r)
    | gh1 == gh2 = normalizeBlockIndex ((gh1, chunks1 >< chunks2) :<| r)
    | otherwise = (gh1, chunks1) :<| normalizeBlockIndex ((gh2, chunks2) :<| r)
normalizeBlockIndex l = l

-- |Export a database in V3 format, as a collection of block file chunks, given the data directory
-- root and the export directory root.
exportDatabaseV3 ::
    (MonadIO m, MonadLogger m, MonadMask m) =>
    -- |Data directory
    FilePath ->
    -- |Export directory
    FilePath ->
    -- |Chunk size
    Word64 ->
    m Bool
exportDatabaseV3 dbDir outDir chunkSize = do
    let indexFile = outDir </> "blocks.idx"
    indexFileExists <- liftIO $ doesFileExist indexFile
    -- ensure that the user-provided database directory exists
    dbDirExists <- liftIO $ doesDirectoryExist dbDir
    unless dbDirExists (liftIO . fail $ "Database directory '" <> dbDir <> "' does not exist")
    -- the following invariant is true: all blocks for which
    --  - its genesis index < genIndex, or
    --  - its genesis index = genindex and its height < startHeight
    -- are previously exported and contained in chunks accounted
    -- for in blockIndex. The file corresponding to the last chunk
    -- entry of blockIndex contains the blocks at height > startHeight-1
    -- at genesis index genIndex. This chunk contains no finalization
    -- records. lastChunkFileM is the filename of the most recently
    -- exported chunk containing all previously exported blocks of
    -- genesis index == genIndex and height >= startHeight as well
    -- as finalization records, if such a file exists.
    (genIndex, startHeight, blockIndex, lastChunkFilenameM) <-
        if indexFileExists
            then do
                indexContents <- liftIO $ readFile indexFile
                blockIndex <- case AP.parseOnly parseBlockIndexFile (T.pack indexContents) of
                    Left err ->
                        liftIO $
                            fail $
                                "An error occurred while parsing '"
                                    <> indexFile
                                    <> "': "
                                    <> err
                    -- normalize the block index; see `normalizeBlockIndex`
                    -- for an explanation of why this is done.
                    Right bi -> do
                        return $ normalizeBlockIndex bi
                case blockIndex of
                    -- we export blocks starting from height 1
                    -- because genesis blocks need not be exported.
                    Empty -> return (0, 1, Empty, Nothing)
                    blockIndexInit :|> (gd, chunks) -> case chunks of
                        Empty ->
                            liftIO $
                                fail $
                                    "A section for genesis hash '"
                                        <> show gd
                                        <> "' in block index file has no entries."
                        initChunks :|> BlockIndexChunkInfo{..} -> do
                            -- the filename of the most recently exported chunk.
                            let lastChunkFile = outDir </> T.unpack filename
                            return
                                ( genesisIndex,
                                  blockHeightFirst,
                                  blockIndexInit :|> (gd, initChunks),
                                  Just lastChunkFile
                                )
            else do
                liftIO $ createDirectoryIfMissing True outDir
                -- we export blocks starting from height 1
                -- because genesis blocks need not be exported.
                return (0, 1, Empty, Nothing)

    (exportError, blockIndexTail) <-
        exportSections
            dbDir
            outDir
            chunkSize
            genIndex
            startHeight
            blockIndex
            lastChunkFilenameM

    -- write the block index if anything was exported and
    -- normalize the block index; see `normalizeBlockIndex`
    -- for an explanation of why this is done.
    unless (null blockIndexTail) $ liftIO $ writeFile indexFile $ showBlockIndexFile $ normalizeBlockIndex $ blockIndex <> blockIndexTail

    -- in case something was exported, delete the replaced last chunk.
    case lastChunkFilenameM of
        Just lastChunkFilename -> do
            lastChunkFileExists <- liftIO $ doesFileExist lastChunkFilename
            when (lastChunkFileExists && not (null blockIndexTail)) $ liftIO $ removeFile lastChunkFilename
            return exportError
        Nothing -> return exportError

-- |Return the last exported genesis hash or the provided one.
exportedGenHashOr :: BlockHash -> BlockIndex -> BlockHash
exportedGenHashOr bh blockIndex = case blockIndex of
    -- nothing was previously exported for the
    -- section corresponding to the current
    -- genesis index.
    Empty -> bh
    -- blocks were previously exported for the
    -- current genesis index, so use the genesis
    -- block hash that was previously exported
    -- for that section and compare it with that
    -- of the database.
    _ :|> (gh, _) -> gh

-- |Export blocks from a 'ConsensusV1' database.
exportConsensusV1Blocks ::
    forall pv m.
    ( IsProtocolVersion pv,
      MonadIO m,
      KonsensusV1.MonadTreeStateStore m,
      MonadLogger m,
      MPV m ~ pv
    ) =>
    -- |Export path.
    FilePath ->
    -- |Chunk size
    Word64 ->
    -- |The genesis index.
    GenesisIndex ->
    -- |Height to start export from.
    BlockHeight ->
    -- |The block index of the previous export.
    BlockIndex ->
    -- |Last written chunk in previous export
    Maybe String ->
    -- |Returns a @Bool@ which indicates whether anything went wrong,
    -- i.e. it is 'True' if an error occured and otherwise 'False,
    -- and the resulting 'BlockIndex'.
    m (Bool, BlockIndex)
exportConsensusV1Blocks outDir chunkSize genIndex startHeight blockIndex lastWrittenChunkM = do
    KonsensusV1.lookupFirstBlock >>= \case
        Nothing -> do
            logEvent External LLError "Could not read from database."
            return (True, Empty)
        Just genesisBlock -> do
            let exportedGenHash = exportedGenHashOr genHash blockIndex
                genHash = getHash genesisBlock
            if genHash /= exportedGenHash
                then do
                    logEvent External LLError "Genesis hash does not match the recently exported block index."
                    return (True, Empty)
                else do
                    KonsensusV1.lookupLastBlock >>= \case
                        Nothing -> do
                            logEvent External LLError "Cannot read last block of the database."
                            return (True, Empty)
                        Just sb -> do
                            let getBlockAt height =
                                    KonsensusV1.lookupBlockByHeight height >>= \case
                                        Nothing -> return Nothing
                                        Just b | KonsensusV1.NormalBlock signedBlock <- KonsensusV1.stbBlock b -> do
                                            let serializedBlock = runPut $ KonsensusV1.putSignedBlock signedBlock
                                            return $ Just (serializedBlock, Nothing)
                                        _ -> return Nothing -- Do not export genesis blocks.
                                getFinalizationRecordAt :: FinalizationIndex -> m (Maybe BS.ByteString)
                                getFinalizationRecordAt _ = return Nothing
                            chunks <-
                                writeChunks
                                    genIndex
                                    (demoteProtocolVersion (protocolVersion @pv))
                                    genHash
                                    startHeight
                                    (KonsensusV1.bmHeight . KonsensusV1.stbInfo $ sb)
                                    outDir
                                    chunkSize
                                    lastWrittenChunkM
                                    -- A dummy @FinalizationIndex@ that is not used when
                                    -- exporting a consensusv1 database.
                                    -- However it is required when exporting a consensusv0 database,
                                    -- so in order to reuse the code that exports blocks, then this
                                    -- dummy value is passed in.
                                    0
                                    getBlockAt
                                    getFinalizationRecordAt
                            return (False, singleton (genHash, chunks))

-- |Export database sections corresponding to blocks with genesis indices >= genIndex
-- and of height >= startHeight.
-- Returns a @Bool@ and a @BlockIndex@ where the former indicates whether an error occurred,
-- and the latter contains information about the sections that were succesfully written to the
-- file-system. If a section could not be exported or if any errors occurred this will be logged
-- to `stdout` in this function.
exportSections ::
    (MonadIO m, MonadLogger m, MonadMask m) =>
    -- |Database directory
    FilePath ->
    -- |Export directory
    FilePath ->
    -- |Chunk size in blocks
    Word64 ->
    -- |Genesis index to export
    GenesisIndex ->
    -- |Height of first block in section to export
    BlockHeight ->
    -- |Block index of previously exported blocks for the current genesis index
    BlockIndex ->
    -- |Filename of last chunk in previous export
    Maybe String ->
    m (Bool, BlockIndex)
exportSections dbDir outDir chunkSize genIndex startHeight blockIndex lastWrittenChunkM = do
    let treeStateDir = dbDir </> "treestate-" ++ show genIndex
    -- Check if the database exists for this genesis index.
    dbEx <- liftIO $ doesPathExist $ treeStateDir </> "data.mdb"
    if dbEx
        then -- Open the databases by a trial and error approach since
        -- the meta data store for each of the databases have different types,
        -- and ultimately we would end up initializing the database connections the same
        -- amount of times.

            (liftIO . openReadOnlyDatabase) treeStateDir >>= \case
                Nothing -> do
                    (liftIO . KonsensusV1.openReadOnlyDatabase) treeStateDir >>= \case
                        -- If the database is unrecognized we stop here.
                        Nothing -> do
                            logEvent External LLError $ "Tree state database could not be opened: " <> show treeStateDir
                            return (True, Empty)
                        Just (KonsensusV1.VersionDatabaseHandlers (dbh :: KonsensusV1.DatabaseHandlers pv)) ->
                            runReaderT
                                ( KonsensusV1.runDiskLLDBM $ do
                                    res <- exportConsensusV1Blocks @pv outDir chunkSize genIndex startHeight blockIndex lastWrittenChunkM
                                    liftIO $ KonsensusV1.closeDatabase dbh
                                    return res
                                )
                                dbh
                Just (VersionDatabaseHandlers (dbh :: DatabaseHandlers pv ())) -> do
                    (exportError, sectionData) <- do
                        (liftIO . getLastBlock) dbh >>= \case
                            Left err -> do
                                logEvent External LLError $ "Database section " ++ show genIndex ++ " cannot be exported: " ++ err
                                return (True, Empty)
                            Right (_, sb) -> do
                                evalStateT
                                    ( do
                                        mgenFinRec <- resizeOnResized $ readFinalizationRecord 0
                                        case mgenFinRec of
                                            Nothing -> do
                                                logEvent External LLError "No finalization record found in database for finalization index 0."
                                                return (True, Empty)
                                            Just genFinRec -> do
                                                -- if something was previously exported for the current genesis
                                                -- index, check that the genesis block hash matches that of the
                                                -- database so we do not accidentally export one chain on top of
                                                -- another.
                                                let genHash = finalizationBlockPointer genFinRec
                                                    exportedGenHash = exportedGenHashOr genHash blockIndex
                                                if genHash /= exportedGenHash
                                                    then do
                                                        logEvent External LLError $
                                                            "Genesis blockhash '"
                                                                <> show exportedGenHash
                                                                <> "' of most recently exported genesis index "
                                                                <> "does not match genesis blockhash '"
                                                                <> show genHash
                                                                <> "' found in node database for genesis index '"
                                                                <> show genIndex
                                                                <> "'"
                                                        return (True, Empty)
                                                    else do
                                                        let getBlockAt height =
                                                                resizeOnResized (readFinalizedBlockAtHeight height) >>= \case
                                                                    Nothing -> return Nothing
                                                                    Just b | NormalBlock normalBlock <- sbBlock b -> do
                                                                        let serializedBlock = runPut $ putVersionedBlock (protocolVersion @pv) normalBlock
                                                                            finalizationIndexM = case blockFields normalBlock of
                                                                                Nothing -> Nothing
                                                                                Just fields -> case blockFinalizationData fields of
                                                                                    BlockFinalizationData fr -> Just $ finalizationIndex fr
                                                                                    NoFinalizationData -> Nothing
                                                                        return $ Just (serializedBlock, finalizationIndexM)
                                                                    _ -> return Nothing -- don't export genesis blocks.
                                                            getFinalizationRecordAt finIndex =
                                                                resizeOnResized (readFinalizationRecord finIndex) >>= \case
                                                                    Nothing -> return Nothing
                                                                    Just fr -> return $ Just $ runPut $ putVersionedFinalizationRecordV0 fr
                                                        chunks <-
                                                            writeChunks
                                                                genIndex
                                                                (demoteProtocolVersion (protocolVersion @pv))
                                                                genHash
                                                                startHeight
                                                                (_bpHeight . sbInfo $ sb)
                                                                outDir
                                                                chunkSize
                                                                lastWrittenChunkM
                                                                -- An initial value for the @FinalizationIndex@. This will be updated
                                                                -- as we export blocks with a finalization index assoicated.
                                                                -- The finalization record itself is only exported if the block it
                                                                -- points to is the last block of the section, hence 0 is a reasonable
                                                                -- value for starting the export.
                                                                0
                                                                getBlockAt
                                                                getFinalizationRecordAt
                                                        return (False, singleton (genHash, chunks))
                                    )
                                    (DBState dbh)
                    liftIO $ closeDatabase dbh
                    -- if an error occurred, return sections
                    -- that were succesfully written to the
                    -- file system; otherwise export the section
                    -- corresponding to the incremented genesis
                    -- index.
                    if exportError
                        then return (True, sectionData)
                        else do
                            (err, secs) <-
                                exportSections
                                    dbDir
                                    outDir
                                    chunkSize
                                    (genIndex + 1)
                                    1
                                    Empty
                                    Nothing
                            return (err, sectionData >< secs)
        else do
            -- this is not an error condition, but rather
            -- the condition for terminating the export.
            logEvent External LLError $ "The tree state database does not exist at " ++ treeStateDir
            return (False, Empty)

-- |Action for getting a block (and possibly a finalization index) at a particular height.
-- Note that for 'ConsensusV1' (>P5) the finalization index is simply @Nothing@ as they do not
-- exist on this version.
--
-- Note. This is a slightly hacky abstraction for retrieving blocks, (and their associated @FinalizationIndex@
-- for protocols <P5), but this is a way of abstracting over the two consensus versions and reusing
-- as much functionality for the actual file writing as possible.
type GetBlockAt m = BlockHeight -> m (Maybe (BS.ByteString, Maybe FinalizationIndex))

-- |Action for getting a finalization record at a particular index.
-- Note that for 'ConsensusV1' (>P5) this always returns @Nothing@.
type GetFinalizationRecordAt m = FinalizationIndex -> m (Maybe BS.ByteString)

-- |Write a database section as a collection of chunks in the specified directory. The last exported
-- chunk (i.e. the one containing the block with the greatest height in the section) also contains
-- finalization records finalizing all blocks after the last block containing a finalization
-- record. Returns a list containing chunk file information for exported chunk files, appearing in
-- the order in which they were exported. Cfr. `BlockIndexChunkInfo` for more information.
-- The @Maybe @ parameter contains the filename to be used for the first chunk to be written, if so
-- provided, and if the file already exists, a version number is added and used instead.
writeChunks ::
    (MonadIO m, MonadLogger m) =>
    -- |Genesis index
    GenesisIndex ->
    -- |Protocol version
    ProtocolVersion ->
    -- |Genesis block hash
    BlockHash ->
    -- |Height of first block in section
    BlockHeight ->
    -- |Height of last block in section
    BlockHeight ->
    -- |Export directory
    FilePath ->
    -- |Chunk size in blocks
    Word64 ->
    -- |Filename of last chunk in previous export
    Maybe String ->
    -- |The last finalization record index.
    FinalizationIndex ->
    -- |Function for getting the serialized block at a
    -- particular height.
    GetBlockAt m ->
    -- |Action for getting the finalization record at
    -- a particular index.
    GetFinalizationRecordAt m ->
    m (Seq BlockIndexChunkInfo)
writeChunks
    sectionGenesisIndex
    sectionProtocolVersion
    sectionGenesisHash
    sectionFirstBlockHeight
    sectionLastBlockHeight
    outDir
    chunkSize
    lastWrittenChunkM
    lastFinalizationRecordIndex
    getBlockAt
    getFinalizationRecordAt = do
        let chunkNameCandidate =
                -- Use the chunk file name if specified and otherwise use a fresh name.
                case lastWrittenChunkM of
                    Just path -> outDir </> path
                    Nothing ->
                        outDir
                            </> "blocks-"
                            ++ show sectionGenesisIndex
                            ++ "-"
                            ++ (show . theBlockHeight) sectionFirstBlockHeight
                            ++ ".dat"

        (chunkName, chunkHdl) <- liftIO $ initialHandle chunkNameCandidate

        (sectionStart, blocksStart) <- liftIO $ do
            BS.hPut chunkHdl $ encode (3 :: Version)
            sectionStart <- hTell chunkHdl
            -- Write a dummy section header that we will later overwrite
            runPutH (liftPut $ putWord64be sectionHeaderLength >> put placeholderSectionHeader) chunkHdl
            blocksStart <- hTell chunkHdl
            return (sectionStart, blocksStart)
        (sectionBlockCount, lastFinalizationRecordIndex') <- exportBlocksToChunk chunkHdl sectionFirstBlockHeight chunkSize lastFinalizationRecordIndex getBlockAt
        blocksEnd <- liftIO $ hTell chunkHdl
        -- Only write finalization records to a chunk if it's the last one for the section
        let lastExportedBlockHeight = sectionFirstBlockHeight + BlockHeight sectionBlockCount - 1
        sectionFinalizationCount <-
            if lastExportedBlockHeight < sectionLastBlockHeight
                then return 0
                else exportFinRecsToChunk chunkHdl lastFinalizationRecordIndex' getFinalizationRecordAt
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
        logEvent External LLInfo $
            "Exported chunk "
                ++ takeFileName chunkName
                ++ " containing blocks with heights from "
                ++ show sectionFirstBlockHeight
                ++ " to "
                ++ (show . theBlockHeight) lastExportedBlockHeight
                ++ " and "
                ++ show sectionFinalizationCount
                ++ " finalization record(s)"
        let chunkInfo =
                BlockIndexChunkInfo
                    (T.pack $ takeFileName chunkName)
                    sectionGenesisIndex
                    sectionFirstBlockHeight
                    (sectionFirstBlockHeight + BlockHeight sectionBlockCount - 1)
        if lastExportedBlockHeight < sectionLastBlockHeight
            then do
                chunks <-
                    writeChunks
                        sectionGenesisIndex
                        sectionProtocolVersion
                        sectionGenesisHash
                        (sectionFirstBlockHeight + BlockHeight chunkSize)
                        sectionLastBlockHeight
                        outDir
                        chunkSize
                        Nothing
                        lastFinalizationRecordIndex'
                        getBlockAt
                        getFinalizationRecordAt
                return $ chunkInfo :<| chunks
            else return $ singleton chunkInfo

-- |Export a series of blocks as a chunk of a specified length. For each block containing a
-- finalization record, the 'dbsLastFinIndex' field of the state is updated with its finalization
-- index.
exportBlocksToChunk ::
    MonadIO m =>
    -- |Handle to export to
    Handle ->
    -- |Height of next block to export
    BlockHeight ->
    -- |Number of blocks to export
    Word64 ->
    -- |Last finalization record index.
    FinalizationIndex ->
    -- |Action for getting the block.
    GetBlockAt m ->
    -- |Number of exported blocks and last finalization record index.
    m (Word64, FinalizationIndex)
exportBlocksToChunk hdl firstHeight chunkSize lastFinalizationRecordIndex getBlockAt = ebtc firstHeight 0 lastFinalizationRecordIndex
  where
    ebtc height count lastFinRecIdx =
        getBlockAt height >>= \case
            Nothing -> return (count, lastFinRecIdx)
            Just (serializedBlock, finalizationRecordIndexM) -> do
                let len = fromIntegral $ BS.length serializedBlock
                liftIO $ do
                    BS.hPut hdl $ runPut $ putWord64be len
                    BS.hPut hdl serializedBlock
                let lastFinRecIdx' = fromMaybe lastFinRecIdx finalizationRecordIndexM
                if count < chunkSize
                    then ebtc (height + 1) (count + 1) lastFinRecIdx'
                    else return (count, lastFinRecIdx')

-- |Export all finalization records with indices above `dbsLastFinIndex` to a chunk
exportFinRecsToChunk ::
    MonadIO m =>
    -- |Handle to export to
    Handle ->
    -- |Last finalization record index
    FinalizationIndex ->
    -- |Action for getting the finalization record at a given index.
    GetFinalizationRecordAt m ->
    -- |Number of exported finalization records
    m Word64
exportFinRecsToChunk hdl finRecIdx getFinalizationRecordAt = exportFinRecsFrom 0 (1 + finRecIdx)
  where
    exportFinRecsFrom count finRecIndex =
        getFinalizationRecordAt finRecIndex >>= \case
            -- if there's no finalization record with an index after the last exported one,
            -- there are no more finalization records to export
            Nothing -> return count
            Just serializedFr -> do
                let len = fromIntegral $ BS.length serializedFr
                liftIO $ do
                    BS.hPut hdl $ runPut $ putWord64be len
                    BS.hPut hdl serializedFr
                exportFinRecsFrom (count + 1) (finRecIndex + 1)

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
            fileSize <- liftIO $ hFileSize hdl
            v <- liftIO $ getVersionBytes hdl
            case decode v of
                Left err -> failWith $ "Error deserializing version header: " ++ err
                Right version
                    | version == supportedVersion -> importSections hdl fileSize
                    | otherwise ->
                        failWith $
                            "Block file version is "
                                ++ show version
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
    importSections hdl fileSize = do
        eof <- liftIO $ hIsEOF hdl
        unless eof $ do
            sectionStart <- liftIO $ hTell hdl
            sectionBS <- getLengthByteString hdl fileSize
            case decode sectionBS of
                Left err -> failWith err
                Right SectionHeader{..} -> do
                    when (sectionGenesisIndex >= firstGenIndex) $ do
                        replicateM_
                            (fromIntegral sectionBlockCount)
                            ( importData hdl fileSize $
                                ImportBlock sectionProtocolVersion sectionGenesisIndex
                            )
                        replicateM_
                            (fromIntegral sectionFinalizationCount)
                            ( importData hdl fileSize $
                                ImportFinalizationRecord sectionProtocolVersion sectionGenesisIndex
                            )
                    -- Move to the next section
                    liftIO $ hSeek hdl AbsoluteSeek (sectionStart + toInteger sectionLength)
                    importSections hdl fileSize
    -- This function takes the file handle, the file size (used for bounds checking reads),
    -- and a function for wrapping the read data as 'ImportData'.
    importData :: Handle -> Integer -> (BS.ByteString -> ImportData) -> ExceptT (ImportFailure a) m ()
    importData hdl fileSize makeImport = do
        blockBS <- getLengthByteString hdl fileSize
        ExceptT $ cbk $ makeImport blockBS
    -- This takes the file handle and the length of the file, which is used for checking that
    -- the length of the byte string is within bounds.
    getLengthByteString :: Handle -> Integer -> ExceptT (ImportFailure a) m BS.ByteString
    getLengthByteString hdl fileSize = do
        lenBS <- liftIO $ BS.hGet hdl 8
        case runGet getWord64be lenBS of
            Right len -> do
                curPos <- liftIO $ hTell hdl
                unless (fromIntegral len <= fileSize - curPos) $ failWith "unexpected end of file"
                bs <- liftIO $ BS.hGet hdl (fromIntegral len)
                if BS.length bs == fromIntegral len
                    then return bs
                    else failWith "unexpected end of file"
            _ -> failWith "unexpected end of file"
