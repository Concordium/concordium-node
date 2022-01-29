{-# LANGUAGE ScopedTypeVariables #-}
module Concordium.GlobalState.ContractStateV1 where

import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Data.Serialize
import Control.Monad.Reader
import Foreign.Ptr
import Foreign.C.Types
import Data.Word
import Data.ByteString.Unsafe (unsafePackCStringFinalizer, unsafeUseAsCStringLen)
import Concordium.Crypto.FFIHelpers (rs_free_array_len)
import Foreign (Storable(peek))
import Foreign.Marshal (alloca)
import System.IO.Unsafe

import Concordium.GlobalState.Persistent.BlobStore
import qualified Data.FixedByteString as FBS
import Concordium.Types.HashableTo (MHashableTo(..), HashableTo (getHash))
import qualified Concordium.Crypto.SHA256 as SHA256
import Data.ByteString.Char8 (ByteString)
import Concordium.Utils.Serialization (putByteStringLen, getByteStringLen)

newtype MutableState = MutableState (ForeignPtr MutableState)

newMutableState :: Ptr MutableState -> IO MutableState
newMutableState ptr = MutableState <$> newForeignPtr freeMutableState ptr

withMutableState :: MutableState -> (Ptr MutableState -> IO a) -> IO a
withMutableState (MutableState fp) = withForeignPtr fp

makePersistent :: TransientState -> PersistentState
makePersistent (TransientState st) = st

newtype PersistentState = PersistentState (ForeignPtr PersistentState)

newtype TransientState = TransientState PersistentState

newtype TransientMutableState = TransientMutableState MutableState

withPersistentState :: PersistentState -> (Ptr PersistentState -> IO a) -> IO a
withPersistentState (PersistentState fp) = withForeignPtr fp

-- |The closure takes a length and a buffer to write into. It is meant to
-- write into the provided buffer.
foreign import ccall "load_persistent_tree_v1" loadPersistentTree :: LoadCallback -> BlobRef PersistentState -> IO (Ptr PersistentState)
foreign import ccall unsafe "&free_persistent_state_v1" freePersistentState :: FunPtr (Ptr PersistentState -> IO ())
foreign import ccall unsafe "&free_mutable_state_v1" freeMutableState :: FunPtr (Ptr MutableState -> IO ())

-- |Write out the tree using the provided callback, and return a pointer to the
-- header.
foreign import ccall "write_persistent_tree_v1" writePersistentTree :: StoreCallback -> Ptr PersistentState -> Ptr CSize -> IO (Ptr Word8)

-- |Freeze the mutable state and compute the root hash. This deallocates the
-- mutable state and writes the hash to the provided pointer, which should be
-- able to hold 32 bytes.
foreign import ccall "freeze_mutable_state_v1" freezePersistentTree :: Ptr MutableState -> Ptr Word8 -> IO (Ptr PersistentState)

-- |Make a fresh mutable state from the persistent one.
foreign import ccall "thaw_persistent_state_v1" thawPersistentTree :: Ptr PersistentState -> IO (Ptr MutableState)

-- |Get the amount of additional space that will be needed to store the new
-- entries.
foreign import ccall "get_new_state_size_v1" getNewStateSizeFFI :: Ptr MutableState -> IO Word64

-- |Cache the persistent state, loading all parts of the tree that are purely on
-- disk.
foreign import ccall "cache_persistent_state_v1" cachePersistentState :: Ptr PersistentState -> IO ()

-- |Compute and retrieve the hash of the persistent state. The function is given
-- a buffer to write the hash into.
foreign import ccall "hash_persistent_state_v1" hashPersistentState :: Ptr PersistentState -> Ptr Word8 -> IO ()

-- |Serialize the persistent state into a byte buffer. The return value is a
-- pointer to the beginning of the buffer, and the second argument is where the
-- length of the buffer is written.
foreign import ccall "serialize_persistent_state_v1" serializePersistentState :: LoadCallback -> Ptr PersistentState -> Ptr CSize -> IO (Ptr Word8)

-- |Deserialize state from a byte buffer.
foreign import ccall "deserialize_persistent_state_v1" deserializePersistentState :: Ptr Word8 -> CSize -> IO (Ptr PersistentState)

{-# NOINLINE getNewStateSize #-}
getNewStateSize :: MutableState -> Word64
getNewStateSize ms = unsafePerformIO (withMutableState ms getNewStateSizeFFI)

unsafeMkForeign :: TransientMutableState -> MutableState
unsafeMkForeign (TransientMutableState ms) = ms

unsafeFromForeign :: MutableState -> TransientMutableState
unsafeFromForeign = TransientMutableState

freeze :: MutableState -> IO (SHA256.Hash, PersistentState)
freeze ms = do
  (psPtr, hashBytes) <- withMutableState ms $ \msPtr -> FBS.createWith (freezePersistentTree msPtr)
  ps <- newForeignPtr freePersistentState psPtr
  return (SHA256.Hash hashBytes, PersistentState ps)

thaw :: PersistentState -> IO MutableState
thaw ms = do
  msPtr <- withPersistentState ms thawPersistentTree
  MutableState <$> newForeignPtr freeMutableState msPtr

{-# NOINLINE freezeTransient #-}
freezeTransient :: TransientMutableState -> (SHA256.Hash, TransientState)
freezeTransient (TransientMutableState tms) = unsafePerformIO $ do
  (h, s) <- freeze tms
  return (h, TransientState s)

{-# NOINLINE thawTransient #-}
thawTransient :: TransientState -> TransientMutableState
thawTransient (TransientState ts) = TransientMutableState . unsafePerformIO $ thaw ts

instance (MonadBlobStore m) => BlobStorable m PersistentState where
  store = fmap fst . storeUpdate

  load = do
    br :: BlobRef PersistentState <- get
    pure $! do
      loadCallback <- fst <$> getCallBacks
      liftIO $
        PersistentState <$> (newForeignPtr freePersistentState =<< loadPersistentTree loadCallback br)

  storeUpdate ps = do
    storeCallback <- snd <$> getCallBacks
    liftIO $ alloca $ \sizePtr -> do
      bytePtr <- withPersistentState ps (\psPtr -> writePersistentTree storeCallback psPtr sizePtr)
      len <- peek sizePtr
      bs <- unsafePackCStringFinalizer (castPtr bytePtr) (fromIntegral len) (rs_free_array_len bytePtr (fromIntegral len))
      return (putByteString bs, ps)

instance MonadBlobStore m => Cacheable m PersistentState where
  cache ps = do
    liftIO (withPersistentState ps cachePersistentState)
    return ps

instance MonadBlobStore m => MHashableTo m SHA256.Hash PersistentState where
  getHashM ps = do
    ((), hsh) <- liftIO (withPersistentState ps $ FBS.createWith . hashPersistentState)
    return (SHA256.Hash hsh)

toByteString :: MonadBlobStore m => PersistentState -> m ByteString
toByteString ps = do
  loadCallback <- fst <$> getCallBacks
  liftIO $ withPersistentState ps $ \psPtr -> alloca $ \sizePtr -> do
    bytePtr <- serializePersistentState loadCallback psPtr sizePtr
    len <- peek sizePtr
    unsafePackCStringFinalizer (castPtr bytePtr) (fromIntegral len) (rs_free_array_len bytePtr (fromIntegral len))

instance HashableTo SHA256.Hash TransientState where
  {-# NOINLINE getHash #-}
  getHash (TransientState ps) = unsafePerformIO $ do
    ((), hsh) <- liftIO (withPersistentState ps $ FBS.createWith . hashPersistentState)
    return (SHA256.Hash hsh)

-- TODO: Make sure to serialize in a way that allows passing the bytestring back to be deserialized,
-- so length + data
instance Serialize TransientState where
  {-# NOINLINE get #-}
  get = do
    bs <- getByteStringLen
    unsafePerformIO $ unsafeUseAsCStringLen bs $ \(bytePtr, len) -> do
      res <- deserializePersistentState (castPtr bytePtr) (fromIntegral len)
      if res == nullPtr then
        return (fail "Could not deserialize state")
      else do
        return . TransientState . PersistentState <$> newForeignPtr freePersistentState res
  {-# NOINLINE put #-}
  put (TransientState ps) = unsafePerformIO $ do
    withPersistentState ps $ \psPtr -> alloca $ \sizePtr -> do
      bytePtr <- serializePersistentState nullFunPtr psPtr sizePtr
      len <- peek sizePtr
      putByteStringLen <$> unsafePackCStringFinalizer (castPtr bytePtr) (fromIntegral len) (rs_free_array_len bytePtr (fromIntegral len))
