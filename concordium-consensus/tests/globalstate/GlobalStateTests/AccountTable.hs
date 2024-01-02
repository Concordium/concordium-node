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
import Data.Word

import Concordium.GlobalState.Persistent.AccountTable

-- | Perform the provided action with a temporary file used for backing the memory mapped bytestring.
withTemporaryMemoryMappedBuffer :: FilePath ->(MemoryMappedBuffer -> IO a) -> IO a
withTemporaryMemoryMappedBuffer tempFileName = bracket openFile closeFile
  where
    openFile = fst <$> openMemoryMappedBuffer tempFileName
    closeFile mmap = do
        closeMemoryMappedBuffer mmap
        removeFile tempFileName

dummyElement :: Element
dummyElement = mkElement $ BS.pack $ replicate elementSize 1

-- | Simple test that creates a memory mapped buffer (with initial size 0, and step size 1).
testMemMappedBuffer :: Assertion
testMemMappedBuffer = withTemporaryMemoryMappedBuffer "mytestmmapbuffer" $ \mmap -> do
    -- append the mmap with an element and check that the returned offset is correct.
    startOffset <- appendWithElement dummyElement mmap
    assertEqual "offset missmatch" 0 startOffset
    -- check that the just appended element can be read correctly.
    element' <- readElement 0 mmap
    assertEqual "elements should match" dummyElement element'
    -- replace the element at index 0
    let newElement = mkElement $ BS.pack $ replicate elementSize 2
    void $ replaceElement 0 newElement mmap
    element'' <- readElement 0 mmap
    assertEqual "replaced bytestrings should match" newElement element''
    -- append again such that the memory mapped file is expanded and remapped.
    let newElement' = mkElement $ BS.pack $ replicate elementSize 3
    appendWithElement newElement' mmap
    element''' <- readElement 1 mmap
    assertEqual "new appended elements should match" newElement' element'''

-- | Generate a list of arbitrary elements together with elements to substitute together with the according indices to replace.
genElements :: Gen ([Element], [(Int, Element)])
genElements = do
    noElements <- choose (0, 100)
    noToReplace <- choose (0, noElements)
    elements <- replicateM noElements (mkElement . BS.pack <$> vector elementSize)
    subElements <- replicateM noToReplace $ do
          elementsToReplace <- (mkElement . BS.pack <$> vector elementSize)
          replaceOffset <- choose (0, noElements)
          return (replaceOffset, elementsToReplace)
    return (elements, subElements)

-- | Test reads, updates and appending of 'MemoryMappedByteString' against a regular 'ByteString'.
testMemMappedByteStringProp :: Spec
testMemMappedByteStringProp = it "test memory mapped bytestring" $ do
    withMaxSuccess 10000 $
        forAll genElements $ \(elements, newElementsAndIndices) -> withTemporaryMemoryMappedBuffer "mytestmmapbytestring2" $ \mmap -> do
            -- apppend all elements to the memory mapping.
            forM_ elements (\element -> appendWithElement element mmap)
            -- Check that the memory mapping corresponds to the list of elements.
            forM_ (zip [0..] elements) $ \(offset, expectedElement) -> do
                actualElement <- readElement offset mmap
                assertEqual "Elements should be the same" expectedElement actualElement

            forM_ newElementsAndIndices $ \(offset, replacement) -> do
                void $ replaceElement (fromIntegral offset) replacement mmap

            -- Check again that the memory mapping corresponds to the list of elements.
            forM_ (zip [0..] elements) $ \(offset, expectedElement) -> do
                actualElement <- readElement offset mmap
                assertEqual "Elements should be the same after replacements" expectedElement actualElement
                
-- -- | Perform the provided action with a temporary file used for backing the 'FlatLFMB'.
-- withTemporaryFlatLFMBTree :: FilePath -> (FlatLFMBTree -> IO a) -> IO a
-- withTemporaryFlatLFMBTree tempFileName = bracket openFile closeFile
--   where
--     openFile = mkFlatLFMBTree tempFileName
--     closeFile flatLfmb = do
--         closeFlatLFMBTree flatLfmb
--         removeFile tempFileName

-- -- | Test getting and setting the metadata for a 'FlatLFMBTree'
-- testGetAndSetMetadata :: Assertion
-- testGetAndSetMetadata = withTemporaryFlatLFMBTree "testFlatLFMBTree" $ \tree -> do
--     metadata <- getMetadata 8 tree
--     assertEqual "Unexpected placeholder FlatLFMBTreeMetadata" (FlatLFMBTreeMetadata 0) metadata
--     void $ setMetadata (FlatLFMBTreeMetadata 1) tree
--     metadata' <- getMetadata 8 tree
--     assertEqual "Unexpected FlatLFMBTreeMetadata" (FlatLFMBTreeMetadata 1) metadata'

-- todo: write a test that makes sure remapping and growing of file works properly
tests :: Spec
tests = describe "AccountTable" $ do
    it "open append, replace and read from memory mapped buffer" testMemMappedBuffer
    testMemMappedByteStringProp
    -- it "get and set FlatLFMBTreeMetadata" testGetAndSetMetadata
