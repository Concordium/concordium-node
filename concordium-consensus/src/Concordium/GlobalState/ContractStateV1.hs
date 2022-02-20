{-# LANGUAGE ScopedTypeVariables #-}
module Concordium.GlobalState.ContractStateV1
 (
   PersistentState,
   TransientState,
   MutableState,
   TransientMutableState,
   newMutableState,
   makePersistent,
   unsafeMkForeign,
   unsafeFromForeign,
   withMutableState,
   getNewStateSize,
   freeze,
   thaw,
   freezeTransient,
   thawTransient,
   toByteString,
   -- * Testing
   lookupKey
 )
where

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

import Concordium.GlobalState.ContractStateFFIHelpers (StoreCallback, LoadCallback, errorLoadCallBack)
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

-- |Write out the tree using the provided callback, and return a BlobRef to the root.
foreign import ccall "store_persistent_tree_v1" storePersistentTree :: StoreCallback -> Ptr PersistentState -> IO (BlobRef PersistentState)

-- |Freeze the mutable state and compute the root hash. This deallocates the
-- mutable state and writes the hash to the provided pointer, which should be
-- able to hold 32 bytes.
foreign import ccall "freeze_mutable_state_v1" freezePersistentTree :: LoadCallback -> Ptr MutableState -> Ptr Word8 -> IO (Ptr PersistentState)

-- |Make a fresh mutable state from the persistent one.
foreign import ccall "thaw_persistent_state_v1" thawPersistentTree :: Ptr PersistentState -> IO (Ptr MutableState)

-- |Get the amount of additional space that will be needed to store the new
-- entries.
foreign import ccall "get_new_state_size_v1" getNewStateSizeFFI :: LoadCallback -> Ptr MutableState -> IO Word64

-- |Cache the persistent state, loading all parts of the tree that are purely on
-- disk.
foreign import ccall "cache_persistent_state_v1" cachePersistentState :: LoadCallback -> Ptr PersistentState -> IO ()

-- |Compute and retrieve the hash of the persistent state. The function is given
-- a buffer to write the hash into.
foreign import ccall "hash_persistent_state_v1" hashPersistentState :: Ptr PersistentState -> Ptr Word8 -> IO ()

-- |Serialize the persistent state into a byte buffer. The return value is a
-- pointer to the beginning of the buffer, and the second argument is where the
-- length of the buffer is written.
foreign import ccall "serialize_persistent_state_v1" serializePersistentState :: LoadCallback -> Ptr PersistentState -> Ptr CSize -> IO (Ptr Word8)

-- |Deserialize state from a byte buffer.
foreign import ccall "deserialize_persistent_state_v1" deserializePersistentState :: Ptr Word8 -> CSize -> IO (Ptr PersistentState)

-- |Functions that exist only for testing.
{-# WARNING persistentStateV1Lookup "Not efficient. DO NOT USE IN PRODUCTION." #-}
foreign import ccall "persistent_state_v1_lookup" persistentStateV1Lookup
   :: LoadCallback
   -> Ptr Word8
   -> CSize -- ^ Pointer to the beginning of the key and its length.
   -> Ptr PersistentState
   -> Ptr CSize -- ^Length of the output data, if the output pointer is not null.
   -> IO (Ptr Word8)

{-# WARNING lookupKey "Not efficient. DO NOT USE IN PRODUCTION." #-}
lookupKey :: TransientState -> ByteString -> IO (Maybe ByteString)
lookupKey (TransientState ps) k =
  withPersistentState ps $ \psPtr ->
    unsafeUseAsCStringLen k $ \(keyPtr, keyLen) ->
      alloca $ \outPtr -> do
        res <- persistentStateV1Lookup errorLoadCallBack (castPtr keyPtr) (fromIntegral keyLen) psPtr outPtr
        if res == nullPtr then return Nothing
        else do
          len <- peek outPtr
          Just <$> unsafePackCStringFinalizer (castPtr res) (fromIntegral len) (rs_free_array_len res (fromIntegral len))

{-# NOINLINE getNewStateSize #-}
getNewStateSize :: LoadCallback -> MutableState -> Word64
getNewStateSize cbk ms = unsafePerformIO (withMutableState ms (getNewStateSizeFFI cbk))

unsafeMkForeign :: TransientMutableState -> MutableState
unsafeMkForeign (TransientMutableState ms) = ms

unsafeFromForeign :: MutableState -> TransientMutableState
unsafeFromForeign = TransientMutableState

freeze :: LoadCallback -> MutableState -> IO (SHA256.Hash, PersistentState)
freeze callbacks ms = do
  (psPtr, hashBytes) <- withMutableState ms $ \msPtr -> FBS.createWith (freezePersistentTree callbacks msPtr)
  ps <- newForeignPtr freePersistentState psPtr
  return (SHA256.Hash hashBytes, PersistentState ps)

thaw :: PersistentState -> IO MutableState
thaw ms = do
  msPtr <- withPersistentState ms thawPersistentTree
  MutableState <$> newForeignPtr freeMutableState msPtr

{-# NOINLINE freezeTransient #-}
freezeTransient :: TransientMutableState -> (SHA256.Hash, TransientState)
freezeTransient (TransientMutableState tms) = unsafePerformIO $ do
  (h, s) <- freeze errorLoadCallBack tms
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
    liftIO $ do
      bRef <- withPersistentState ps $ storePersistentTree storeCallback
      return (put bRef, ps)

instance MonadBlobStore m => Cacheable m PersistentState where
  cache ps = do
    (cbk, _) <- getCallBacks
    liftIO (withPersistentState ps (cachePersistentState cbk))
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
      bytePtr <- serializePersistentState errorLoadCallBack psPtr sizePtr
      len <- peek sizePtr
      putByteStringLen <$> unsafePackCStringFinalizer (castPtr bytePtr) (fromIntegral len) (rs_free_array_len bytePtr (fromIntegral len))
