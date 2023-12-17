-- | Tests for the account table and related abstrations.
module GlobalStateTests.AccountTable (tests) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as BS
import System.Directory
import Test.HUnit
import Test.Hspec
import Test.QuickCheck

import Concordium.GlobalState.Persistent.AccountTable

-- | Perform the provided action with a temporary file used for backing the memory mapped bytestring.
withTemporaryMemoryMappedByteString :: FilePath -> (MemoryMappedByteString -> IO a) -> IO a
withTemporaryMemoryMappedByteString tempFileName action = bracket openFile closeFile action
  where
    openFile = openMemoryMappedByteString tempFileName
    closeFile mmap = do
        closeMemoryMappedByteString mmap
        removeFile tempFileName

-- | Test simple open mmap bytestring, close mmmap bytestring, appending and reading from memory mapped bytestring.
testMemMappedByteString :: Assertion
testMemMappedByteString = withTemporaryMemoryMappedByteString "mytestmmapbytestring" $ \mmap -> do
    let bs = BS.pack $ replicate 64 0
    startOffset <- appendBytesToMemoryMappedByteString bs mmap
    assertEqual "offset missmatch" 0 startOffset
    bs' <- readFromMemoryMappedByteString (0, 32) mmap
    assertEqual "bytestrings should match" (BS.take 32 bs) bs'

-- | Generate arbitrary byte strings.
genByteString :: Gen [BS.ByteString]
genByteString = do
    bssLen <- choose (0, 10)
    bsLen <- choose (0, 128)
    replicateM bssLen $ BS.pack <$> vector bsLen

-- | Test reads and appending of 'MemoryMappedByteString' against a regular 'ByteString'.
testMemMappedByteStringProp :: Spec
testMemMappedByteStringProp = it "test memory mapped bytestring" $ do
    withMaxSuccess 10000 $
        forAll genByteString $ \bss -> withTemporaryMemoryMappedByteString "mytestmmapbytestring2" $ \mmap -> do
            let bs = BS.empty
            finalBs <- foldM (\acc newBs -> (addBs mmap) acc newBs) bs bss
            bs' <- readFromMemoryMappedByteString (0, fromIntegral $ BS.length finalBs) mmap
            assertEqual "ByteString and MemoryMappedByteString does not have same length" (BS.length finalBs) (BS.length bs')
            assertEqual "ByteString and MemoryMappedByteString does not match" finalBs bs'
  where
    addBs mmap acc bs = do
        void $ appendBytesToMemoryMappedByteString bs mmap
        return $ acc <> bs

tests :: Spec
tests = describe "AccountTable" $ do
    it "open append and read from memory mapped bytestring" testMemMappedByteString
    testMemMappedByteStringProp
