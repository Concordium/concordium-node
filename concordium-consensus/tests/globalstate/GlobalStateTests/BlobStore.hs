{-# LANGUAGE RankNTypes #-}

-- |Tests for implementation of 'MonadBlobStore'.
-- These tests generate random sequences of the basic operations defined by 'MonadBlobStore' that
-- have a well-defined semantics, and ensure that their behaviour is as expected.
-- Here, well-defined means:
--
-- - Reads from the blob store must read 'BlobRef's that were created with previous stores.
-- - Reads of 'BlobPtr's must be 'BlobPtr's that refer to a subset of the data of a 'BlobRef';
--   that is, the start offset should be at least 8 + the address of the 'BlobRef', and the
--   length plus the start offset should not exceed the end of the 'BlobRef'.
--
-- Both the 'BlobStore' and 'MemBlobStore' implementations are tested.
module GlobalStateTests.BlobStore where

import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as Unsafe
import qualified Data.Sequence as Seq
import Data.Void
import Data.Word
import Foreign
import Foreign.C.Types
import Test.HUnit
import Test.Hspec
import Test.QuickCheck

import Concordium.GlobalState.ContractStateFFIHelpers
import Concordium.GlobalState.Persistent.BlobStore
import Control.Exception

data BlobStoreOperation
    = -- |Finish the operations
      Done
    | -- |Store the given bytestring
      Store BS.ByteString BlobStoreOperation
    | -- |Load the nth most recent store and check it is correct. Does nothing if there is no
      -- store with that index.
      Load Int BlobStoreOperation
    | -- |Call 'flushStore'
      Flush BlobStoreOperation
    | -- |Call 'loadBlobPtr' on a 'BlobPtr' derived from the nth most recent stored blob ref,
      -- given an offset and length. The offset and length will be capped so they cannot exceed
      -- the length of the stored bytestring.
      LoadBlobPtr Int Word64 Word64 BlobStoreOperation
    | -- |Call 'getCallbacks'.
      GetCallbacks BlobStoreOperation
    | -- |Store via the nth most recent call to 'getCallbacks'.
      StoreViaCallbacks Int BS.ByteString BlobStoreOperation
    | -- |Load via the nth most recent call to 'getCallbacks' the mth most recently stored 'BlobRef'.
      -- The first argument is the index of the callbacks to use.
      LoadViaCallbacks Int Int BlobStoreOperation
    deriving (Show)

-- |Drop the loads that load from a particular store given the index. This decrements the
-- references to stores at higher indexes. (Used for shrinking 'BlobStoreOperation'.)
dropLoads :: Int -> BlobStoreOperation -> BlobStoreOperation
dropLoads _ Done = Done
dropLoads i (Store bs cont) = Store bs (dropLoads (i + 1) cont)
dropLoads i (Load j cont)
    | i < j = Load (j - 1) (dropLoads i cont)
    | i == j = dropLoads i cont
    | otherwise = Load j (dropLoads i cont)
dropLoads i (Flush cont) = Flush (dropLoads i cont)
dropLoads i (LoadBlobPtr j off len cont)
    | i < j = LoadBlobPtr (j - 1) off len (dropLoads i cont)
    | i == j = dropLoads i cont
    | otherwise = LoadBlobPtr j off len (dropLoads i cont)
dropLoads i (GetCallbacks cont) = GetCallbacks (dropLoads i cont)
dropLoads i (StoreViaCallbacks n bs cont) = StoreViaCallbacks n bs (dropLoads (i + 1) cont)
dropLoads i (LoadViaCallbacks n j cont)
    | i < j = LoadViaCallbacks n (j - 1) (dropLoads i cont)
    | i == j = dropLoads i cont
    | otherwise = LoadViaCallbacks n j (dropLoads i cont)

-- |Drop loads and stores that use a particular callbacks index. This decrements the references
-- to callbacks at higher indexes. (Used for shrinking 'BlobStoreOperation'.)
dropCallbacks :: Int -> BlobStoreOperation -> BlobStoreOperation
dropCallbacks _ Done = Done
dropCallbacks i (Store bs cont) = Store bs (dropCallbacks i cont)
dropCallbacks i (Load n cont) = Load n (dropCallbacks i cont)
dropCallbacks i (Flush cont) = Flush (dropCallbacks i cont)
dropCallbacks i (LoadBlobPtr j off len cont) = LoadBlobPtr j off len (dropCallbacks i cont)
dropCallbacks i (GetCallbacks cont) = GetCallbacks (dropCallbacks (i + 1) cont)
dropCallbacks i (StoreViaCallbacks j bs cont)
    | i < j = StoreViaCallbacks (j - 1) bs (dropCallbacks i cont)
    | i == j = dropLoads 0 $ dropCallbacks i cont -- Note: since we drop the store, we also drop any loads from this
    | otherwise = StoreViaCallbacks j bs (dropCallbacks i cont)
dropCallbacks i (LoadViaCallbacks j m cont)
    | i < j = LoadViaCallbacks (j - 1) m (dropCallbacks i cont)
    | i == j = dropCallbacks i cont
    | otherwise = LoadViaCallbacks j m (dropCallbacks i cont)

-- |A shrink function for 'BlobStoreOperation' that will remove a single operation and any
-- subsequent operations that depend on it. QuickCheck uses this to minimise counterexamples in
-- the event of test failure.
shrinkBlobStoreOperation :: BlobStoreOperation -> [BlobStoreOperation]
shrinkBlobStoreOperation Done = []
shrinkBlobStoreOperation (Store bs cont) =
    dropLoads 0 cont : (Store bs <$> shrinkBlobStoreOperation cont)
shrinkBlobStoreOperation (Load j cont) = cont : (Load j <$> shrinkBlobStoreOperation cont)
shrinkBlobStoreOperation (Flush cont) = cont : (Flush <$> shrinkBlobStoreOperation cont)
shrinkBlobStoreOperation (LoadBlobPtr j off len cont) = cont : (LoadBlobPtr j off len <$> shrinkBlobStoreOperation cont)
shrinkBlobStoreOperation (GetCallbacks cont) = dropCallbacks 0 cont : (GetCallbacks <$> shrinkBlobStoreOperation cont)
shrinkBlobStoreOperation (StoreViaCallbacks i bs cont) = dropLoads 0 cont : (StoreViaCallbacks i bs <$> shrinkBlobStoreOperation cont)
shrinkBlobStoreOperation (LoadViaCallbacks i j cont) = cont : (LoadViaCallbacks i j <$> shrinkBlobStoreOperation cont)

-- |Generator for 'BlobStoreOperation' given a context of:
--   - lengths of previously stored blobs (most recent first)
--   - number of previous calls to 'getCallbacks'
genBlobStoreOperation' :: Seq.Seq Int -> Int -> Gen BlobStoreOperation
genBlobStoreOperation' stored cbks = do
    sz <- getSize
    oneof $ genDone : if sz > 0 then ops else []
  where
    ops = [genStore, genFlush, genCallbacks] ++ loadOps ++ viaCbks
    loadOps
        | Seq.null stored = []
        | otherwise = [genLoad, genLoadBlobPtr]
    viaCbks
        | cbks == 0 = []
        | Seq.null stored = [genStoreViaCallbacks]
        | otherwise = [genStoreViaCallbacks, genLoadViaCallbacks]
    genDone = return Done
    genStore = do
        bs <- BS.pack <$> arbitrary
        cont <- genBlobStoreOperation' (BS.length bs Seq.<| stored) cbks
        return $ Store bs cont
    genLoad = do
        l <- chooseInt (0, Seq.length stored - 1)
        cont <- genBlobStoreOperation' stored cbks
        return $ Load l cont
    genFlush = Flush <$> genBlobStoreOperation' stored cbks
    genLoadBlobPtr = do
        l <- chooseInt (0, Seq.length stored - 1)
        let len = Seq.index stored l
        offset <- chooseInt (0, len)
        takeLen <- chooseInt (0, len - offset)
        cont <- genBlobStoreOperation' stored cbks
        return $ LoadBlobPtr l (fromIntegral offset) (fromIntegral takeLen) cont
    genCallbacks = do
        cont <- genBlobStoreOperation' stored (cbks + 1)
        return $ GetCallbacks cont
    genStoreViaCallbacks = do
        i <- chooseInt (0, cbks - 1)
        bs <- BS.pack <$> arbitrary
        cont <- genBlobStoreOperation' (BS.length bs Seq.<| stored) cbks
        return $ StoreViaCallbacks i bs cont
    genLoadViaCallbacks = do
        i <- chooseInt (0, cbks - 1)
        j <- chooseInt (0, Seq.length stored - 1)
        cont <- genBlobStoreOperation' stored cbks
        return $ LoadViaCallbacks i j cont

-- |Generator for 'BlobStoreOperation' that should ensure that invariants are established.
genBlobStoreOperation :: Gen BlobStoreOperation
genBlobStoreOperation = genBlobStoreOperation' Seq.empty 0

instance Arbitrary BlobStoreOperation where
    arbitrary = genBlobStoreOperation
    shrink = shrinkBlobStoreOperation

-- |Check that a 'BlobStoreOperation' satisfies basic invariants.
bsoOKTest :: BlobStoreOperation -> Bool
bsoOKTest = tst Seq.empty 0
  where
    tst _ _ Done = True
    tst s n (Store bs cont) = tst (bs Seq.<| s) n cont
    tst s n (Load i cont) = 0 <= i && i < Seq.length s && tst s n cont
    tst s n (Flush cont) = tst s n cont
    tst s n (LoadBlobPtr j off len cont) = case Seq.lookup j s of
        Nothing -> False
        Just bs ->
            0 <= off
                && 0 <= len
                && off <= fromIntegral (BS.length bs)
                && off + len <= fromIntegral (BS.length bs)
                && tst s n cont
    tst s n (GetCallbacks cont) = tst s (n + 1) cont
    tst s n (StoreViaCallbacks i bs cont) =
        0 <= i && i < n && tst (bs Seq.<| s) n cont
    tst s n (LoadViaCallbacks i j cont) =
        0 <= i && i < n && 0 <= j && j < Seq.length s && tst s n cont

foreign import ccall "dynamic" callLoadCallback :: LoadCallback -> LoadCallbackType
foreign import ccall "dynamic" callStoreCallback :: StoreCallback -> StoreCallbackType

foreign import ccall "return_value_to_byte_array" vectorToByteArray :: Ptr Vec -> Ptr CSize -> IO (Ptr Word8)
foreign import ccall unsafe "free_array_len" freeByteArray :: Ptr Word8 -> Word64 -> IO ()
foreign import ccall unsafe "box_vec_u8_free" freeVector :: Ptr Vec -> IO ()

-- |Run a 'BlobStoreOperation' in a monad that implements 'MonadBlobStore', given:
-- - A context of 'BlobRef's that have already been written, with their contents.
-- - A context of callbacks that have already been obtained from calls to 'getCallbacks'.
runBlobStoreOperation' :: (MonadBlobStore m) => Seq.Seq (BlobRef Void, BS.ByteString) -> Seq.Seq (LoadCallback, StoreCallback) -> BlobStoreOperation -> m ()
runBlobStoreOperation' _ _ Done = return ()
runBlobStoreOperation' s cbks (Store bs cont) = do
    r <- storeRaw bs
    runBlobStoreOperation' ((r, bs) Seq.<| s) cbks cont
runBlobStoreOperation' s cbks (Load i cont) = do
    let (r, expect) = Seq.index s i
    actual <- loadRaw r
    liftIO $ assertEqual "Loaded BlobRef" expect actual
    runBlobStoreOperation' s cbks cont
runBlobStoreOperation' s cbks (Flush cont) = do
    flushStore
    runBlobStoreOperation' s cbks cont
runBlobStoreOperation' s cbks (LoadBlobPtr i start len cont) = do
    let (BlobRef base, bs) = Seq.index s i
    let bp = BlobPtr (base + 8 + start) len
    let expect = BS.take (fromIntegral len) $ BS.drop (fromIntegral start) bs
    actual <- loadBlobPtr bp
    liftIO $ assertEqual "Loaded BlobPtr" expect actual
    runBlobStoreOperation' s cbks cont
runBlobStoreOperation' s cbks (GetCallbacks cont) = do
    cb <- getCallbacks
    runBlobStoreOperation' s (cb Seq.<| cbks) cont
runBlobStoreOperation' s cbks (StoreViaCallbacks i bs cont) = do
    let (_, storeCbk) = Seq.index cbks i
    r <- liftIO $ Unsafe.unsafeUseAsCStringLen bs $ \(cs, len) ->
        callStoreCallback storeCbk (castPtr cs) (fromIntegral len)
    runBlobStoreOperation' ((BlobRef r, bs) Seq.<| s) cbks cont
runBlobStoreOperation' s cbks (LoadViaCallbacks i j cont) = do
    let (loadCbk, _) = Seq.index cbks i
    let (BlobRef r, expect) = Seq.index s j
    liftIO $ do
        vec <- callLoadCallback loadCbk r
        actual <- alloca $ \lenPtr -> do
            ba <- vectorToByteArray vec lenPtr
            len <- peek lenPtr
            Unsafe.unsafePackCStringFinalizer ba (fromIntegral len) (freeByteArray ba (fromIntegral len))
        freeVector vec
        assertEqual "Loaded BlobRef" expect actual
    runBlobStoreOperation' s cbks cont

-- |Run a 'BlobStoreOperation' in a monad that implements 'MonadBlobStore'.
runBlobStoreOperation :: (MonadBlobStore m) => BlobStoreOperation -> m ()
runBlobStoreOperation = runBlobStoreOperation' Seq.empty Seq.empty

-- |Run a 'BlobStoreOperation' in the 'MemBlobStore'.
testMemBlobStore :: BlobStoreOperation -> Property
testMemBlobStore bso = ioProperty $ bracket newMemBlobStore (\_ -> return ()) $ \mbs -> do
    runMemBlobStoreT (runBlobStoreOperation bso) mbs

-- |Run a 'BlobStoreOperation' in a 'BlobStore'.
testBlobStore :: BlobStoreOperation -> Property
testBlobStore bso = ioProperty $ runBlobStoreTemp "." $ runBlobStoreOperation bso

tests :: Spec
tests = describe "BlobStore" $ do
    -- Test that generators satisfy the invariants.
    it "GenOK" $ forAll genBlobStoreOperation bsoOKTest
    -- Test that shrinking preserves the invariants.
    it "ShrinkOK" $ forAll genBlobStoreOperation (all bsoOKTest . shrink)
    -- Test the memory blob store.
    it "MemBlobStore" $ property testMemBlobStore
    -- Test the disk blob store.
    it "BlobStore" $ property testBlobStore
