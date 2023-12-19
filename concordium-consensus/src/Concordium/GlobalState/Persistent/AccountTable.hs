{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

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

import Concordium.Crypto.ByteStringHelpers
import qualified Concordium.Crypto.SHA256 as H
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.Types

import Concordium.GlobalState.Persistent.Account

-- * Memory mapped byte string which serves as a backing store for the 'AccountTable'.

data MemoryMappedFileHandle = MemoryMappedFileHandle
    { -- | Handle for the memory mapped file.
      mmfhHandle :: !Handle,
      -- | Path to the memory mapped file.
      mmfhFilePath :: !FilePath,
      -- | Size of the file
      mmfhSize :: !Int
    }
    deriving (Eq, Show)

-- | A 'ByteString' which is memory mapped via the handle @mmFileHandle@.
--  The caller must make sure that writes are sequential
data MemoryMappedByteString = MemoryMappedByteString
    { -- | The contents of the memory mapped file.
      mmapRef :: !(IORef BS.ByteString),
      -- | The underlying file that is memory mapped.
      mmFileHandle :: !(IORef MemoryMappedFileHandle)
    }
    deriving (Eq)

newtype MemoryMappedByteStringError = MemoryMappedByteStringError String
    deriving (Eq, Show, Typeable)

instance Exception MemoryMappedByteStringError where
    displayException (MemoryMappedByteStringError reason) =
        "MemoryMappedByteString error: "
            ++ show reason

-- | A range (start, end) to read from a memory mapped bytestring, where @start@ and @end@ are absolute offsets.
--  In order to be well formed, then the first component MUST be less than or equal to the second component.
type Range = (Word64, Word64)

-- | Return the length of the file of the provided file handle.
readFileLength :: MemoryMappedFileHandle -> IO Word64
readFileLength MemoryMappedFileHandle{..} = mask $ \restore -> do
    res <- try $ restore $ do
        hSeek mmfhHandle SeekFromEnd 0
        hTell mmfhHandle
    case res :: Either SomeException Integer of
        Left err -> throwM err
        Right len -> return $ fromIntegral len

-- | Open a 'MemoryMappedByteString' at the provided 'FilePath'.
--  If the file does not exist, then it is created.
--  Returns the 'MemoryMapppedByteString' in the first component and the
--  size of the file in the second component.
openMemoryMappedByteString :: FilePath -> IO (MemoryMappedByteString, Word64)
openMemoryMappedByteString mmfhFilePath = do
    mmfhHandle <- openBinaryFile mmfhFilePath ReadWriteMode
    mmfhSize <- fromIntegral <$> hFileSize mmfhHandle
    mmFileHandle <- newIORef MemoryMappedFileHandle{..}
    mmapRef <- newIORef =<< mmapFileByteString mmfhFilePath Nothing
    return (MemoryMappedByteString{..}, fromIntegral mmfhSize)

-- | Close a 'MemoryMappedByteString' and flushing the memory mapped @ByteString@
--  to disk in the process.
closeMemoryMappedByteString :: MemoryMappedByteString -> IO ()
closeMemoryMappedByteString MemoryMappedByteString{..} = do
    hdl <- readIORef mmFileHandle
    hClose $ mmfhHandle hdl

-- | Read a 'Range' of the supplied memory mapped byte string.
-- The supplied @Range@ must be well formed i.e., the second component must be greater than or equal to the first component.
readByteString :: Range -> MemoryMappedByteString -> IO BS.ByteString
readByteString (start, end) MemoryMappedByteString{..} = do
    mmap <- readIORef mmapRef
    if fromIntegral end > BS.length mmap
        then do
            mmap' <- tryRemap
            writeIORef mmapRef mmap'
            return $ BS.take (fromIntegral end) $ BS.drop (fromIntegral start) mmap'
        else return $ BS.take (fromIntegral end) $ BS.drop (fromIntegral start) mmap
  where
    -- Remap the file if the read is within bounds.
    tryRemap = do
        mmfh@MemoryMappedFileHandle{..} <- readIORef mmFileHandle
        fileLength <- readFileLength mmfh
        if fromIntegral end > fileLength
            then throwM . MemoryMappedByteStringError $ "Out of bounds read"
            else mmapFileByteString mmfhFilePath Nothing

-- | Append the provided 'ByteString' to the supplied 'MemoryMappedByteString'.
--  Return the offset of the byte string.
appendWithByteString ::
    -- | The 'ByteString' to append.
    BS.ByteString ->
    -- | The memory mapped byte string which is appended to.
    MemoryMappedByteString ->
    -- | The offset of the supplied 'ByteString' in the memory mapped byte string.
    IO Word64
appendWithByteString bs MemoryMappedByteString{..} = do
    mmfh@MemoryMappedFileHandle{..} <- readIORef mmFileHandle
    hSeek mmfhHandle SeekFromEnd 0
    BS.hPut mmfhHandle bs
    writeIORef mmFileHandle mmfh{mmfhSize = mmfhSize + BS.length bs}
    return $ fromIntegral mmfhSize

-- | Overwrite the 'MemoryMappedByteString' starting at the provided offset with the provided 'ByteString'.
--
-- The starting offset must be within the existing bytestring, and so must the offset plus
-- the size of the bytestring which is inserted.
replaceByteString ::
    -- | The offset
    Word64 ->
    -- | The new bytestring
    BS.ByteString ->
    -- | The memory mapped bytestring
    MemoryMappedByteString ->
    IO ()
replaceByteString offset bs MemoryMappedByteString{..} = do
    MemoryMappedFileHandle{..} <- readIORef mmFileHandle
    if fromIntegral offset + BS.length bs > fromIntegral mmfhSize
        then throwM . MemoryMappedByteStringError $ "Out of bounds replacing"
        else do
            hSeek mmfhHandle AbsoluteSeek (fromIntegral offset)
            BS.hPut mmfhHandle bs

-- | A flattened left full merkle binary tree.
--  Nodes and leaves are stored in-order.
--
--  Invariants:
--    * The tree must always be non-empty.
data FlatLFMB = FlatLFMB
    { -- | The underlying memory mapped byte string that
      --  holds onto the contents of the tree.
      flfmbMmap :: !MemoryMappedByteString,
      -- | The current height of the tree.
      flfmbHeight :: !Word64
    }

-- | Size of each node in 'FlatLFMB'
--  8 bytes for the blob ref (word64) plus 32 bytes for the sha256 hash.
nodeSize :: Int
nodeSize = 40

-- | Type used for encoding the size of a node.
data NodeSize
    deriving (Typeable, Data)

instance FBS.FixedLength NodeSize where
    fixedLength _ = nodeSize

-- | A node in a 'FlatLFMB'.
newtype Node = Node (FBS.FixedByteString NodeSize)
    deriving (Eq, Ord)
    deriving (Show) via FBSHex NodeSize
    deriving (Serialize) via FBSHex NodeSize

-- | Make a new flattened left full merkle binary tree.
mkFlatLFMB :: FilePath -> Node -> IO FlatLFMB
mkFlatLFMB fp (Node fbs) = do
    (flfmbMmap, _) <- openMemoryMappedByteString fp
    void $ appendWithByteString (FBS.toByteString fbs) flfmbMmap
    return FlatLFMB{flfmbHeight = 0, ..}

-- | Load a 'FlatLFMB' at the provided @FilePath@.
loadFlatLFMB :: FilePath -> IO FlatLFMB
loadFlatLFMB fp = do
    (flfmbMmap, fileSize) <- openMemoryMappedByteString fp
    let flfmbHeight = fileSize `div` fromIntegral nodeSize -- todo: store the height at the start of the file.
    return FlatLFMB{..}

closeFlatLFMB :: FlatLFMB -> IO ()
closeFlatLFMB = undefined

-- | Get the location of the root node in the tree.
--  Precondition: @FlatLFMB@ must be non-empty.
--  The root node is always located at 2^height - 1 in the memory mapped bytestring.
getRootNodeRange :: FlatLFMB -> Range
getRootNodeRange FlatLFMB{..} =
    let offset = (2 ^ flfmbHeight) - (1 :: Word64)
    in  (offset, offset + fromIntegral nodeSize)

-- | Get the location of the hash of the root node.
--  Precondition: @FlatLFMB@ must be non-empty.
getRootNodeHashRange :: FlatLFMB -> Range
getRootNodeHashRange flatLFMB =
    let (rootStart, rootEnd) = getRootNodeRange flatLFMB
    in  (rootStart + 8, rootEnd)

-- | Get the reference of the root node.
--  Precondition: @FlatLFMB@ must be non-empty.
getRootNodeReferenceRange :: FlatLFMB -> Range
getRootNodeReferenceRange flatLFMB =
    let (rootStart, _) = getRootNodeRange flatLFMB
    in  (rootStart, rootStart + 8)

-- | Get the root hash of the merkle tree.
--  Precondition: @FlatLFMB@ must be non-empty.
getRootHash :: FlatLFMB -> IO H.Hash
getRootHash flatLFMB@FlatLFMB{..} = do
    let rootHash = getRootNodeHashRange flatLFMB
    bs <- readByteString rootHash flfmbMmap
    return $ H.Hash $ FBS.fromByteString bs

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

getAccountEntry :: (MonadIO m) => AccountTable -> AccountIndex -> m (AccountTableEntry a)
getAccountEntry AccountTable{..} (AccountIndex ai) = do
    arr <- liftIO $ readIORef ataMmap
    let entryBytes = BS.take (fromIntegral nodeSize) $ BS.drop accountOffset arr
    case decode entryBytes of
        Left err -> undefined -- todo error handling
        Right a -> return a
  where
    -- As accounts are stored at the leaves of the binary tree.
    -- then they are located at every other entry of the array.
    accountOffset = fromIntegral ai * 2
