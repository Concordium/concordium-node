-- |Functionality for importing and exporting the block database.
--
-- The block database format is as follows:
--
-- * Version header (Version - variable length): 3
-- * One or more sections
--
-- Each section consists of:
--
-- * The length of the section including the header (Word64be)
-- * The genesis index of blocks in the section (Word32be)
-- * The genesis block hash (32 bytes)
-- * The block height of the first block (Word64be)
-- * The number of blocks present (Word64be)
-- * The total length of the block portion (Word64be)
-- * The number of finalization records (Word64be)
-- * The block portion, consisting for each block of:
--    - The length of the serialized block (Word64be)
--    - The serialized block data
-- * The finalization record portion, consisting for each finalization record of:
--    - The length of the finalization record (Word64be)
--    - The serialized finalization record
--
-- Within a section, the blocks must be sequential and of the correct number.
-- The finalization records must also be sequential and of the correct number.
-- The finalization records must finalize blocks that are included in the section
-- for which there is not a finalization record included in another block.
--
-- Sections themselves should be ordered with sequential genesis indexes.
module Concordium.ImportExport where

import Data.Serialize
import Data.Word
import System.IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Concordium.Types
import Concordium.Utils.Serialization.Put

-- |A section header of an exported block database
data SectionHeader = SectionHeader
    { sectionLength :: !Word64,
      sectionGenesisIndex :: !GenesisIndex,
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
        put sectionGenesisHash
        put sectionFirstBlockHeight
        putWord64be sectionBlockCount
        putWord64be sectionBlocksLength
        putWord64be sectionFinalizationCount
    get = do
        sectionLength <- getWord64be
        sectionGenesisIndex <- get
        sectionGenesisHash <- get
        sectionFirstBlockHeight <- get
        sectionBlockCount <- getWord64be
        sectionBlocksLength <- getWord64be
        sectionFinalizationCount <- getWord64be
        return SectionHeader{..}

dummySectionHeader :: SectionHeader
dummySectionHeader = SectionHeader 0 0 (BlockHash minBound) 0 0 0 0

-- |Write a section to the file handle.  It must be possible to write and seek the handle.
writeSection ::
    -- |Genesis index
    GenesisIndex ->
    -- |Genesis block hash
    BlockHash ->
    -- |Height of first block in section
    BlockHeight ->
    -- |Handle to write to
    Handle ->
    -- |Action to write out the blocks and return the number of them
    IO Word64 ->
    -- |Action to write out the finalization records and return the number of them
    IO Word64 ->
    IO ()
writeSection
    sectionGenesisIndex
    sectionGenesisHash
    sectionFirstBlockHeight
    hdl
    writeBlocks
    writeFinRecs = do
        sectionStart <- hTell hdl
        -- Write a dummy section header that we will later overwrite
        runPutH (liftPut $ put dummySectionHeader) hdl
        blocksStart <- hTell hdl
        sectionBlockCount <- writeBlocks
        blocksEnd <- hTell hdl
        sectionFinalizationCount <- writeFinRecs
        sectionEnd <- hTell hdl
        -- Go back to the start and rewrite the section header with the correct data
        hSeek hdl AbsoluteSeek sectionStart
        let sectionHeader = SectionHeader{
            sectionLength = fromInteger (sectionEnd - sectionStart),
            sectionBlocksLength = fromInteger (blocksEnd - blocksStart),
            ..}
        runPutH (liftPut $ put sectionHeader) hdl
        hSeek hdl AbsoluteSeek sectionEnd

