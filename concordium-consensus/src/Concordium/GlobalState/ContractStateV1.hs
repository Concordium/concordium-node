{-| This module provides types to represent V1 contract state, in both its
persistent and mutable variants. The persistent variant is there to ensure
sharing and incremental state updates. The mutable variant is designed for
efficient updates during a transaction, including checkpointing and rollbacks.
-}
{-# LANGUAGE ScopedTypeVariables #-}
module Concordium.GlobalState.ContractStateV1
 (
   PersistentState,
   InMemoryPersistentState,
   MutableState(..),
   MutableStateInner,
   newMutableState,
   makePersistent,
   withMutableState,
   getNewStateSize,
   freeze,
   thaw,
   freezeInMemoryPersistent,
   thawInMemoryPersistent,
   toByteString,
   -- * Testing
   lookupKey,
   generatePersistentTree
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

-- |Opaque pointer to the mutable state. This state exists only for the duration
-- of a transaction and is then deallocated by running a finalizer.
newtype MutableStateInner = MutableStateInner (ForeignPtr MutableStateInner)

type MutableStateContext = LoadCallback

-- |Mutable state together with the context that determines how to access any
-- pointers into a persistent part of the state.
data MutableState = MutableState {
  msInner :: !MutableStateInner,
  msContext :: !MutableStateContext
 }

-- |Attach a finalizer to the given allocated opaque mutable state reference.
-- This function can be used at most once on any given pointer, otherwise data
-- that is pointed to will be freed twice, leading to a memory access error.
newMutableState :: MutableStateContext -> Ptr MutableStateInner -> IO MutableState
newMutableState msContext ptr = do
  msInner <- MutableStateInner <$> newForeignPtr freeMutableState ptr
  return MutableState{..}

-- |Get temporary access to the mutable state pointer.
withMutableState :: MutableState -> (Ptr MutableStateInner -> IO a) -> IO a
withMutableState MutableState{msInner=MutableStateInner fp} = withForeignPtr fp

-- |Convert an in-memory persistent state to a normal persistent state.
makePersistent :: InMemoryPersistentState -> PersistentState
makePersistent (InMemoryPersistentState st) = st

-- |An opaque pointer to a contract instance's state. "Persistent" here is in
-- the sense of "persistent data structures", meaning that the state is not
-- updated in place, but that modifications create a copy of the relevant part
-- of the structure. This state is thus designed so that as little data as
-- possible needs to be modified by updates. An additional feature of this state
-- is that it can be loaded into memory on-demand.
newtype PersistentState = PersistentState (ForeignPtr PersistentState)

-- |An in-memory variant of the 'PeristentState'. No operations ever write any
-- part of the state to disk, and thus the entirety of this state is always
-- in-memory.
newtype InMemoryPersistentState = InMemoryPersistentState PersistentState

-- |Gain temporary access to a pointer to the persistent state.
withPersistentState :: PersistentState -> (Ptr PersistentState -> IO a) -> IO a
withPersistentState (PersistentState fp) = withForeignPtr fp

-- |Load persistent state from the given disk reference. The provided closure is
-- called to read data from persistent storage.
foreign import ccall "load_persistent_tree_v1" loadPersistentTree :: LoadCallback -> BlobRef PersistentState -> IO (Ptr PersistentState)
foreign import ccall unsafe "&free_persistent_state_v1" freePersistentState :: FunPtr (Ptr PersistentState -> IO ())
foreign import ccall unsafe "&free_mutable_state_v1" freeMutableState :: FunPtr (Ptr MutableStateInner -> IO ())

-- |Write out the tree using the provided callback, and return a BlobRef to the root.
foreign import ccall "store_persistent_tree_v1" storePersistentTree :: StoreCallback -> Ptr PersistentState -> IO (BlobRef PersistentState)

-- |Freeze the mutable state and compute the root hash. This leaves the mutable
-- state empty (and thus mutable state should not be used after a call to this
-- function), and writes the hash to the provided pointer, which should be able
-- to hold 32 bytes.
foreign import ccall "freeze_mutable_state_v1" freezePersistentTree :: LoadCallback -> Ptr MutableStateInner -> Ptr Word8 -> IO (Ptr PersistentState)

-- |Make a fresh mutable state from the persistent one.
foreign import ccall "thaw_persistent_state_v1" thawPersistentTree :: Ptr PersistentState -> IO (Ptr MutableStateInner)

-- |Get the amount of additional space that will be needed to store the new
-- entries.
foreign import ccall "get_new_state_size_v1" getNewStateSizeFFI :: LoadCallback -> Ptr MutableStateInner -> IO Word64

-- |Cache the persistent state, loading all parts of the tree that are purely on
-- disk.
foreign import ccall "cache_persistent_state_v1" cachePersistentState :: LoadCallback -> Ptr PersistentState -> IO ()

-- |Compute and retrieve the hash of the persistent state. The function is given
-- a buffer to write the hash into.
foreign import ccall "hash_persistent_state_v1" hashPersistentState :: Ptr PersistentState -> Ptr Word8 -> IO ()

-- |Serialize the persistent state into a byte buffer. The return value is a
-- pointer to the beginning of the buffer, and the last argument is where the
-- length of the buffer is written.
foreign import ccall "serialize_persistent_state_v1" serializePersistentState :: LoadCallback -> Ptr PersistentState -> Ptr CSize -> IO (Ptr Word8)

-- |Deserialize state from a byte buffer.
foreign import ccall "deserialize_persistent_state_v1" deserializePersistentState :: Ptr Word8 -> CSize -> IO (Ptr PersistentState)

{-# NOINLINE getNewStateSize #-}
-- |Get the amount of additional data that will be needed to store the given
-- mutable state. This function is only called at the end of a transaction and
-- may mutate the mutable state so that further operations, in particular
-- @freeze@ are more efficient.
getNewStateSize :: MutableState -> Word64
getNewStateSize ms = unsafePerformIO (withMutableState ms (getNewStateSizeFFI (msContext ms)))

-- |Freeze the mutable state into a persistent state, computing its hash on the
-- way.
freeze :: LoadCallback -> MutableState -> IO (SHA256.Hash, PersistentState)
freeze callbacks ms = do
  (psPtr, hashBytes) <- withMutableState ms $ \msPtr -> FBS.createWith (freezePersistentTree callbacks msPtr)
  ps <- newForeignPtr freePersistentState psPtr
  return (SHA256.Hash hashBytes, PersistentState ps)

-- |Convert the persistent state to a mutable one.
thaw :: MutableStateContext -> PersistentState -> IO MutableState
thaw msContext ms = do
  msPtr <- withPersistentState ms thawPersistentTree
  msInner <- MutableStateInner <$> newForeignPtr freeMutableState msPtr
  return MutableState{..}

{-# NOINLINE freezeInMemoryPersistent #-}
-- |A specialization of 'freeze', assuming that the mutable state has all data in-memory.
freezeInMemoryPersistent :: MutableState -> (SHA256.Hash, InMemoryPersistentState)
freezeInMemoryPersistent ms =
  let (hsh, s) = unsafePerformIO $ freeze errorLoadCallBack ms
  in (hsh, InMemoryPersistentState s)

{-# NOINLINE thawInMemoryPersistent #-}
-- |A specialization of 'thaw' above, assuming that the persistent state has all data in-memory.
thawInMemoryPersistent :: InMemoryPersistentState -> MutableState
thawInMemoryPersistent (InMemoryPersistentState ts) = unsafePerformIO $ thaw errorLoadCallBack ts

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

instance HashableTo SHA256.Hash InMemoryPersistentState where
  {-# NOINLINE getHash #-}
  getHash (InMemoryPersistentState ps) = unsafePerformIO $ do
    ((), hsh) <- liftIO (withPersistentState ps $ FBS.createWith . hashPersistentState)
    return (SHA256.Hash hsh)

instance Serialize InMemoryPersistentState where
  {-# NOINLINE get #-}
  get = do
    bs <- getByteStringLen
    unsafePerformIO $ unsafeUseAsCStringLen bs $ \(bytePtr, len) -> do
      res <- deserializePersistentState (castPtr bytePtr) (fromIntegral len)
      if res == nullPtr then
        return (fail "Could not deserialize state")
      else do
        return . InMemoryPersistentState . PersistentState <$> newForeignPtr freePersistentState res
  {-# NOINLINE put #-}
  put (InMemoryPersistentState ps) = unsafePerformIO $ do
    withPersistentState ps $ \psPtr -> alloca $ \sizePtr -> do
      bytePtr <- serializePersistentState errorLoadCallBack psPtr sizePtr
      len <- peek sizePtr
      putByteStringLen <$> unsafePackCStringFinalizer (castPtr bytePtr) (fromIntegral len) (rs_free_array_len bytePtr (fromIntegral len))

{-# WARNING generatePersistentTreeFFI "Only for testing. DO NOT USE IN PRODUCTION." #-}
foreign import ccall "generate_persistent_state_from_seed" generatePersistentTreeFFI :: Word64 -> Word64 -> IO (Ptr PersistentState)

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
-- |Lookup a value in the persistent in-memory state. This function is subject
-- to stack overflow for maliciously constructed states, so must not be used in
-- production.
lookupKey :: InMemoryPersistentState -> ByteString -> IO (Maybe ByteString)
lookupKey (InMemoryPersistentState ps) k =
  withPersistentState ps $ \psPtr ->
    unsafeUseAsCStringLen k $ \(keyPtr, keyLen) ->
      alloca $ \outPtr -> do
        res <- persistentStateV1Lookup errorLoadCallBack (castPtr keyPtr) (fromIntegral keyLen) psPtr outPtr
        if res == nullPtr then return Nothing
        else do
          len <- peek outPtr
          Just <$> unsafePackCStringFinalizer (castPtr res) (fromIntegral len) (rs_free_array_len res (fromIntegral len))

{-# WARNING generatePersistentTree "Only for testing. DO NOT USE IN PRODUCTION." #-}
{-# NOINLINE generatePersistentTree #-}
-- |Generate a random-looking tree from the given seed. This uses SHA512
-- repeatedly, starting from the serialization of the seed, to construct a tree
-- of keys and values. Can only be used for testing.
generatePersistentTree ::
  Word64 -- ^Seed.
  -> Word64 -- ^Number of values.
  -> InMemoryPersistentState
generatePersistentTree seed len = unsafePerformIO $ do
  res <- generatePersistentTreeFFI seed len
  if res == nullPtr then
    error "Could not generate tree."
  else
    InMemoryPersistentState . PersistentState <$> newForeignPtr freePersistentState res

-- |Convert the persistent state to a byte array. This is used during a protocol
-- update to construct the new genesis block. This method must be compatible
-- with the @get@ method of the serialization instance of
-- 'InMemoryPersistentState'.
toByteString :: MonadBlobStore m => PersistentState -> m ByteString
toByteString ps = do
  loadCallback <- fst <$> getCallBacks
  liftIO $ withPersistentState ps $ \psPtr -> alloca $ \sizePtr -> do
    bytePtr <- serializePersistentState loadCallback psPtr sizePtr
    len <- peek sizePtr
    unsafePackCStringFinalizer (castPtr bytePtr) (fromIntegral len) (rs_free_array_len bytePtr (fromIntegral len))
