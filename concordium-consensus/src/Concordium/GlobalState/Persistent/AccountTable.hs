{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Concordium.GlobalState.Persistent.AccountTable where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import Data.Data
import qualified Data.FixedByteString as FBS
import Data.IORef
import qualified Data.IntMap.Strict as IntMap
import Data.Serialize
import Data.Word
import System.IO
import System.IO.MMap
import Foreign hiding (void)

import Concordium.Crypto.ByteStringHelpers
import qualified Concordium.Crypto.SHA256 as H
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.Types

import Concordium.GlobalState.Persistent.Account

-- | Size of each node in 'FlatLFMBTree'
--  8 bytes for the 'BlobRef'plus 32 bytes for the sha256 hash.
elementSize :: Int
elementSize = 40

-- | Type used for encoding the size of a node.
data ElementSize
    deriving (Typeable, Data)

instance FBS.FixedLength ElementSize where
    fixedLength _ = elementSize

-- | An element in the memory mapped region with a fixed size.
newtype Element = Element (FBS.FixedByteString ElementSize)
    deriving (Eq, Ord, Storable)
    deriving (Show) via FBSHex ElementSize
    deriving (Serialize) via FBSHex ElementSize

mkElement :: BS.ByteString -> Element
mkElement = Element <$> FBS.fromByteString

-- * Memory mapped buffer which serves as a backing store for the 'AccountTable'.

-- | The raw handle for a memory mapped file.
data MemoryMappedFileHandle = MemoryMappedFileHandle
    { -- | Path to the memory mapped file.
      mmfhFilePath :: !FilePath,
      -- | Pointer to the memory mapped region.
      mmfhPtr :: !(Ptr Element),
      -- | Raw size of the file (can be longer than @mmfhSize@ due to alignment).
      mmfhRawSize :: !Int,
      -- | Offset for data in the memory mapped region.
      mmfhOffset :: !Int,
      -- | Size of the file in number of bytes.
      mmfhSize :: !Int,
      -- | Number of elements present in the mapping.
      --  If @mmfhNextElement == mmfhSize / elementSize@ then
      --  it is necessary to expand the underlying file when appending.
      mmfhNoElements :: !Int
    }
    deriving (Eq, Show)

-- | A 'ByteString' which is memory mapped via the handle @mmFileHandle@.
--  The caller must make sure that writes are sequential
data MemoryMappedBuffer = MemoryMappedBuffer
    { -- | The underlying file that is memory mapped.
      mmFileHandle :: !(IORef MemoryMappedFileHandle)
    }
    deriving (Eq)

newtype MemoryMappedBufferError = MemoryMappedBufferError String
    deriving (Eq, Show, Typeable)

instance Exception MemoryMappedBufferError where
    displayException (MemoryMappedBufferError reason) =
        "MemoryMappedBuffer error: "
            ++ show reason

-- | Open a 'MemoryMappedBuffer' at the provided 'FilePath'.
--  If the file does not exist, then it is created and initialized with @stepSize * elementSize@ size (in bytes).
--  Returns the 'MemoryMapppedByteString' in the first component and the
--  size of the file in the second component.
openMemoryMappedBuffer :: FilePath -> IO (MemoryMappedBuffer, Word64)
openMemoryMappedBuffer mmfhFilePath = do
    hdl <- openBinaryFile mmfhFilePath ReadWriteMode
    hdlSize <- hFileSize hdl
    when (hdlSize == 0) $
        BS.hPut hdl $ BS.replicate (fromIntegral defaultStepSize) 0
    -- close and flush the handle so we can mmap the file.
    void $ hClose hdl
    (mmfhPtr, mmfhRawSize, mmfhOffset, mmfhSize) <- mmapFilePtr mmfhFilePath ReadWrite Nothing
    mmFileHandle <- newIORef MemoryMappedFileHandle{mmfhNoElements = 0, ..}
    return (MemoryMappedBuffer{..}, fromIntegral mmfhSize)

-- | Close a 'MemoryMappedBuffer' and flushing the memory mapped @ByteString@
--  to disk in the process.
closeMemoryMappedBuffer :: MemoryMappedBuffer -> IO ()
closeMemoryMappedBuffer MemoryMappedBuffer{..} = do
    MemoryMappedFileHandle{..} <- readIORef mmFileHandle
    void $ munmapFilePtr mmfhPtr mmfhRawSize

-- | Read an 'Element' at the supplied offset.
--
--  Note that this is a partial function and may throw an exception if a read
--  beyond the size (no. elements) is attempted.
readElement ::
    -- | Element offset
    Word64 ->
    -- | The memory mapped 'ByteString' to read from.
    MemoryMappedBuffer ->
    -- | The resulting 'ByteString'.
    IO Element
readElement offset MemoryMappedBuffer{..} = do
    MemoryMappedFileHandle{..} <- readIORef mmFileHandle
    if fromIntegral offset > (mmfhSize `div` elementSize)
        then throwM . MemoryMappedBufferError $ "Out of bounds read"
        else peekElemOff mmfhPtr $ fromIntegral offset
             
-- | Append the provided 'Element' to the supplied 'MemoryMappedBuffer'.
--  Return the offset of element.
--
--  Note that this function may remap the underlying file if appending beyond
--  the original mapping.
appendWithElement ::
    -- | The 'Element' to append.
    Element ->
    -- | The memory mapped byte string which is appended to.
    MemoryMappedBuffer ->
    -- | The offset of the supplied 'ByteString' in the memory mapped byte string.
    IO Word64
appendWithElement element MemoryMappedBuffer{..} = do
    mmfh@MemoryMappedFileHandle{..} <- readIORef mmFileHandle
    when (mmfhSize == 0 || mmfhNoElements >= mmfhSize `div` elementSize) $ do
        void $ munmapFilePtr mmfhPtr mmfhRawSize
        void $ BS.appendFile mmfhFilePath $ BS.replicate (fromIntegral defaultStepSize) 0
        (mmfhPtr', mmfhRawSize', mmfhOffset', mmfhSize') <- mmapFilePtr mmfhFilePath ReadWrite Nothing
        writeIORef mmFileHandle
            MemoryMappedFileHandle
                { mmfhPtr = mmfhPtr',
                  mmfhRawSize = mmfhRawSize',
                  mmfhOffset = mmfhOffset',
                  mmfhSize = mmfhSize',
                  ..
                }
    void $ pokeElemOff mmfhPtr mmfhNoElements element
    writeIORef mmFileHandle mmfh{mmfhNoElements = mmfhNoElements + 1}
    return $ fromIntegral mmfhNoElements

-- | Overwrite the 'MemoryMappedBuffer' starting at the provided offset with the provided 'ByteString'.
--
-- The starting offset must be within the existing bytestring, and so must the offset plus
-- the size of the bytestring which is inserted.
--
-- Note that replacing any bytes (i.e. not appending) causes a remapping of the memory mapped file in order
-- to ensure consistency between the readonly 'ByteString' and the actual memory mapped file.
replaceElement ::
    -- | The offset of the element to replace.
    Word64 ->
    -- | The new element.
    Element ->
    -- | The memory mapped bytestring.
    MemoryMappedBuffer ->
    IO ()
replaceElement offset element MemoryMappedBuffer{..} = do
    MemoryMappedFileHandle{..} <- readIORef mmFileHandle
    if fromIntegral mmfhSize > fromIntegral offset * elementSize 
        then void $ pokeElemOff mmfhPtr (fromIntegral offset) element
        else throwM . MemoryMappedBufferError $ "Out of bounds replacing"

-- * Flattened left full merkle binary tree which is used for a thawed 'AccountTable'.

-- | A flattened left full merkle binary tree.
newtype FlatLFMBTree = FlatLFMBTree MemoryMappedBuffer


-- | Metadata for a 'FlatLFMBTree'.
--  This data is located in the head of the 'FlatLFMBTree'.
--  NOTE. A 'Serialize' instance for this type must either be the same size or a multiple of an 'Element'.
data FlatLFMBTreeMetadata = FlatLFMBTreeMetadata
    { -- | Height of the block of which the data
      --  in the 'FlatLFMBTree' corresponds to.
      flfmbBlockHeight :: !BlockHeight,
      -- | Height of the block of which the data
      --  in the 'FlatLFMBTree' corresponds to.
      flfmBlockHash :: !BlockHash
    }
    deriving (Eq, Show)

instance Serialize FlatLFMBTreeMetadata where
    get = FlatLFMBTreeMetadata <$> get <*> get

    put FlatLFMBTreeMetadata{..} = do
        put flfmbBlockHeight
        put flfmBlockHash


-- | Create an 'Element' from a 'Metadata'.
metadataToElement :: FlatLFMBTreeMetadata -> Element
metadataToElement = Element <$> FBS.fromByteString . encode

-- | The step size must be a multiple of element size in order to ensure memory alignment.
defaultStepSize :: Word64
defaultStepSize = 10000 * fromIntegral elementSize

-- | Make a new flattened left full merkle binary tree.
--  Note that this function writes a placeholder 'FlatFLMBTreeMetadata' to the 'FlatLFMBTree'.
mkFlatLFMBTree :: FilePath -> IO FlatLFMBTree
mkFlatLFMBTree fp = do
    mmap <- fst <$> openMemoryMappedBuffer fp
    void $ appendWithElement (metadataToElement $ FlatLFMBTreeMetadata 0 $ BlockHash $ H.hash "0") mmap
    return $ FlatLFMBTree mmap

-- | Load a 'FlatLFMBTree' at the provided @FilePath@.
loadFlatLFMBTree :: FilePath -> IO FlatLFMBTree
loadFlatLFMBTree fp = do
    (flfmbMmap, _) <- openMemoryMappedBuffer fp
    return $ FlatLFMBTree flfmbMmap

-- | Close up the underlying file handle used for the memory mapping.
--  The caller shoudl not use the 'FlatLFMBTree' further after this function is called.
closeFlatLFMBTree :: FlatLFMBTree -> IO ()
closeFlatLFMBTree (FlatLFMBTree mmap) = closeMemoryMappedBuffer mmap

-- | Read the 'FlatLFMBTree'
getMetadata ::
    -- | The 'FlatLFMBTree' to read the metadata from.
    FlatLFMBTree ->
    -- | The metadata stored for the 'FlatLFMBTree'.
    IO FlatLFMBTreeMetadata
getMetadata (FlatLFMBTree mmap) = do
    (Element fbs) <- readElement 0 mmap
    case decode $ FBS.toByteString fbs of
        Left err -> error $ "Cannot decode FlatLFMBTreeMetadata: " <> err
        Right metadata -> return metadata

-- | Set the metadata to the provided 'FlatLFMBTree'.
setMetadata ::
    -- | The new metadata.
    FlatLFMBTreeMetadata ->
    -- | The tree where the metadata is replaced.
    FlatLFMBTree ->
    IO ()
setMetadata metadata (FlatLFMBTree mmap) = do
    hdl <- readIORef $ mmFileHandle mmap
    void $ pokeElemOff (mmfhPtr hdl) 0 (metadataToElement metadata)

-- -- | Get the location of the root node in the tree.
-- --  Precondition: @FlatLFMBTree@ must be non-empty.
-- --  The root node is always located at 2^height - 1 in the memory mapped bytestring.
-- getRootNodeRange :: FlatLFMBTree -> Range
-- getRootNodeRange (FlatLFMBTree mmap) =
--     let offset = (2 ^ flfmbHeight) - (1 :: Word64)
--     in  (offset, offset + fromIntegral nodeSize)

-- -- | Get the location of the hash of the root node.
-- --  Precondition: @FlatLFMBTree@ must be non-empty.
-- getRootNodeHashRange :: FlatLFMBTree -> Range
-- getRootNodeHashRange flatLFMB =
--     let (rootStart, rootEnd) = getRootNodeRange flatLFMB
--     in  (rootStart + 8, rootEnd)

-- -- | Get the reference of the root node.
-- --  Precondition: @FlatLFMBTree@ must be non-empty.
-- getRootNodeReferenceRange :: FlatLFMBTree -> Range
-- getRootNodeReferenceRange flatLFMB =
--     let (rootStart, _) = getRootNodeRange flatLFMB
--     in  (rootStart, rootStart + 8)

-- -- | Get the root hash of the merkle tree.
-- --  Precondition: @FlatLFMBTree@ must be non-empty.
-- getRootHash :: FlatLFMBTree -> IO H.Hash
-- getRootHash (FlatLFMBTree mmap) = do
--     let rootHash = getRootNodeHashRange flatLFMB
--     bs <- readByteString rootHash flfmbMmap
--     return $ H.Hash $ FBS.fromByteString bs

-- * Change set

data AccountTableChangeSet (pv :: ProtocolVersion) = AccountTableDifferenceMap
    { atdmLeaves :: !(IntMap.IntMap (AccountRef (AccountVersionFor pv))),
      adtmNodes :: !(IntMap.IntMap H.Hash)
    }
    deriving (Show)

-- * Account table

data AccountTableEntry a = AccountTableEntry
    { -- | The blob ref. The underlying value must be
      --  present in the blobstore.
      ateReference :: !(BlobRef a),
      -- | The hash of the underlying value.
      ateHash :: !H.Hash
    }
    deriving (Eq, Show)

instance Serialize (AccountTableEntry a) where
    put AccountTableEntry{..} = do
        put ateReference
        put ateHash
    get = do
        ateReference <- BlobRef <$> getWord64be
        ateHash <- get
        return AccountTableEntry{..}

data AccountTable = AccountTable
    { -- | read-only contents of the memory mapped file.
      ataMmap :: !(IORef BS.ByteString),
      -- | Handle for the memory mapped file.
      ataHandle :: !(MVar Handle),
      -- | Path to the memory mapped account table
      ataFilePath :: !FilePath
    }

readEntryFromHandle :: (MonadIO m) => AccountTable -> m (AccountTableEntry a)
readEntryFromHandle _ = undefined

-- | Read the 'AccountTableEntry' using the memory map.
-- Fall back to read from the file if the the entry is beyond the bytes.
readEntry :: (MonadIO m) => AccountTable -> m (AccountTableEntry a)
readEntry _ = undefined

-- getAccountEntry :: (MonadIO m) => AccountTable -> AccountIndex -> m (AccountTableEntry a)
-- getAccountEntry AccountTable{..} (AccountIndex ai) = do
--     arr <- liftIO $ readIORef ataMmap
--     let entryBytes = BS.take (fromIntegral nodeSize) $ BS.drop accountOffset arr
--     case decode entryBytes of
--         Left err -> undefined -- todo error handling
--         Right a -> return a
--   where
--     -- As accounts are stored at the leaves of the binary tree.
--     -- then they are located at every other entry of the array.
--     accountOffset = fromIntegral ai * 2
