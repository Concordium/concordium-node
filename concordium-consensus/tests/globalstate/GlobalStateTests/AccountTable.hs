-- | Tests for the account table and related abstrations.
module GlobalStateTests.AccountTable (tests) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as BS
import Data.IORef
import System.Directory
import Test.HUnit hiding (Node)
import Test.Hspec
import Test.QuickCheck

import Concordium.GlobalState.Persistent.AccountTable

-- | Perform the provided action with a temporary file used for backing the memory mapped bytestring.
withTemporaryMemoryMappedByteString :: FilePath -> (MemoryMappedByteString -> IO a) -> IO a
withTemporaryMemoryMappedByteString tempFileName = bracket openFile closeFile
  where
    openFile = fst <$> openMemoryMappedByteString tempFileName
    closeFile mmap = do
        closeMemoryMappedByteString mmap
        removeFile tempFileName

-- | Test simple open mmap bytestring, close mmmap bytestring; replacing, appending and reading from memory mapped bytestring.
testMemMappedByteString :: Assertion
testMemMappedByteString = withTemporaryMemoryMappedByteString "mytestmmapbytestring" $ \mmap -> do
    let bs = BS.pack $ replicate 64 0
    startOffset <- appendWithByteString bs mmap
    assertEqual "offset missmatch" 0 startOffset
    bs' <- readByteString (0, 32) mmap
    assertEqual "bytestrings should match" (BS.take 32 bs) bs'

    void $ replaceByteString 0 (BS.pack $ replicate 10 1) mmap
    bs'' <- readByteString (0, 10) mmap
    assertEqual "replaced bytestrings should match" (BS.pack $ replicate 10 1) bs''

-- | Generate arbitrary byte strings and an offset for appending and replacing the bytestring.
genByteString :: Gen (BS.ByteString, Int, BS.ByteString)
genByteString = do
    bsLen <- choose (0, 256)
    replaceBsLen <- choose (0, bsLen)
    bs <- BS.pack <$> vector bsLen
    replaceBs <- BS.pack <$> vector replaceBsLen
    replaceOffset <- choose (0, bsLen - replaceBsLen)
    return (bs, replaceOffset, replaceBs)

-- | Test reads, updates and appending of 'MemoryMappedByteString' against a regular 'ByteString'.
testMemMappedByteStringProp :: Spec
testMemMappedByteStringProp = it "test memory mapped bytestring" $ do
    withMaxSuccess 10000 $
        forAll genByteString $ \(bs, replaceOffset, newBs) -> withTemporaryMemoryMappedByteString "mytestmmapbytestring2" $ \mmap -> do
            -- start with @bs@
            void $ appendWithByteString bs mmap
            -- append with @newBs@
            (bs', addedBsOffset) <- appendBs mmap newBs bs
            -- Check that the offset reported back is correct, i.e the length of the old bs.
            assertEqual "Wrong offset for the added bytestring" (BS.length bs) (fromIntegral addedBsOffset)
            newBs' <- readByteString (addedBsOffset, fromIntegral $ BS.length newBs) mmap
            assertEqual "ByteString read from mmap should correspond to the one just appended" newBs newBs'
            -- now replace some bytes with @newBs@
            bs'' <- replaceBs mmap (fromIntegral replaceOffset) newBs bs'
            -- Read the whole @ByteString@ from the @MemoryMappedByteString@ and compare with the actual @ByteString@.
            allMmap <- readByteString (0, fromIntegral $ BS.length bs'') mmap
            -- Read the cached size of the memory mapped file.
            hdl <- readIORef $ mmFileHandle mmap
            let cachedSize = mmfhSize hdl
            actualFileSize <- readFileLength hdl
            assertEqual "Actual file size and cached size does not match" actualFileSize (fromIntegral cachedSize)
            assertEqual "ByteString and cached size does not match" (BS.length bs'') (fromIntegral cachedSize)
            assertEqual "ByteString and MemoryMappedByteString does not have same length" (BS.length bs'') (BS.length allMmap)
            assertEqual "ByteString and MemoryMappedByteString does not match" bs'' allMmap
  where
    appendBs mmap bs existingBs = do
        offsetOfAddedBs <- appendWithByteString bs mmap
        return (existingBs <> bs, offsetOfAddedBs)
    replaceBs mmap offset bs existingBs = do
        void $ replaceByteString offset bs mmap
        let (prefix, suffix) = BS.splitAt (fromIntegral offset) existingBs
        return $ prefix <> bs <> BS.drop (BS.length bs) suffix

-- | Perform the provided action with a temporary file used for backing the 'FlatLFMB'.
withTemporaryFlatLFMBTree :: FilePath -> (FlatLFMBTree -> IO a) -> IO a
withTemporaryFlatLFMBTree tempFileName = bracket openFile closeFile
  where
    openFile = mkFlatLFMBTree tempFileName
    closeFile flatLfmb = do
        closeFlatLFMBTree flatLfmb
        removeFile tempFileName

-- | Test getting and setting the metadata for a 'FlatLFMBTree'
testGetAndSetMetadata :: Assertion
testGetAndSetMetadata = withTemporaryFlatLFMBTree "testFlatLFMBTree" $ \tree -> do
    metadata <- getMetadata 8 tree
    assertEqual "Unexpected placeholder FlatLFMBTreeMetadata" (FlatLFMBTreeMetadata 0) metadata
    void $ setMetadata (FlatLFMBTreeMetadata 1) tree
    metadata' <- getMetadata 8 tree
    assertEqual "Unexpected FlatLFMBTreeMetadata" (FlatLFMBTreeMetadata 1) metadata'

tests :: Spec
tests = describe "AccountTable" $ do
    it "open append, replace and read from memory mapped bytestring" testMemMappedByteString
    testMemMappedByteStringProp
    it "get and set FlatLFMBTreeMetadata" testGetAndSetMetadata
