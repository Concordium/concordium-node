{-# LANGUAGE
        ScopedTypeVariables,
        DefaultSignatures,
        TypeFamilies,
        TemplateHaskell
#-}
{- |This module provides abstractions for working with LMDB databases.
    Chief among them is the 'MDBDatabase' class, which can be used for
    type-safe access to an 'MDB_dbi'' by providing an implementation.
    The 'Cursor' type and the 'withCursor' and 'getCursor' operations
    provide a type-safe abstraction over cursors.
-}
module Concordium.GlobalState.LMDB.Helpers where

import Control.Concurrent (runInBoundThread)
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Except
import qualified Data.Serialize as S
import Database.LMDB.Raw
import Data.Maybe
import Data.Proxy
import Data.Coerce
import Data.ByteString
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Word
import Foreign.Marshal.Utils
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Lens.Micro.Platform


-- Implementation of reader-writer locks that strongly prefer writers.

data RWState =
   -- |Nobody has acquired the lock.
  Free { waitingWriters :: !Word64 }
  -- |The given number of readers have acquired the lock.
  | ReadLocked {
      readers :: !Word64,
      waitingWriters :: !Word64
      }
  -- |The lock is acquired by a single writer.
  | WriteLocked {
      waitingWriters :: !Word64
      }
    deriving(Show)

-- |A reader writer lock with a bias for writers. If there are waiting writers
-- readers will block.
data RWLock = RWLock {
  state :: !(MVar RWState),
  -- |Used to signal waiting readers to wake up. This is empty when there is at
  -- least one active reader and full otherwise.
  readLock :: !(MVar ()),
  -- |Used to signal waiting writers to wake up. This is empty when there is an
  -- active writer, and full otherwise.
  writeLock :: !(MVar ())
  }

-- |Initialize a lock in the unlocked state.
initializeLock :: IO RWLock
initializeLock = do
  state <- newMVar (Free 0)
  readLock <- newMVar ()
  writeLock <- newMVar ()
  return RWLock{..}

-- |Acquire a read lock. This will block until there are no pending writers
-- waiting to acquire the lock.
acquireRead :: RWLock -> IO ()
acquireRead RWLock{..} = mask_ go
  where
    go = takeMVar state >>= \case
      st@(Free waitingWriters)
        | waitingWriters == 0 -> do
            -- the lock is free and there are no waiting writers. Acquire a read lock.
            takeMVar readLock
            putMVar state (ReadLocked 1 0)
        | otherwise -> do
            -- the lock is free, but there are waiting writers. We do nothing and try again.
            -- Due to fairness of MVars next time another thread will make progress
            -- so we are going to end up after a finite number of iterations, in a WriteLocked state.
            putMVar state st
            go
      st@(ReadLocked n waitingWriters)
          | waitingWriters == 0 ->
            -- No waiting writers, add another reader.
            putMVar state $! ReadLocked (n + 1) 0
          | otherwise -> do
              -- Some readers hold the lock, but there are waiting writers.
              -- We do nothing and wait until there are no more readers and attempt again.
              -- At that point we are likely to end up in WriteLocked state.
              putMVar state st
              readMVar readLock
              go
      lockState@(WriteLocked _) -> do
        -- There is an active writer. Do nothing and wait until the writer is done.
        putMVar state lockState
        readMVar writeLock
        go


acquireWrite :: RWLock -> IO ()
acquireWrite RWLock{..} = mask_ $ go False
  where
    -- the boolean flag indicates whether this is a first iteration of the loop (False) or not (True)
    go b = takeMVar state >>= \case
      (Free waitingWriters) -> do
        -- The lock is free, take it.
        takeMVar writeLock
        putMVar state $! WriteLocked (waitingWriters - if b then 1 else 0)
      (ReadLocked n waitingWriters) -> do
        -- There are active readers. Queue ourselves up and wait until all existing readers
        -- are done. This will block all subsequent readers from acquiring the lock.
        putMVar state $! ReadLocked n (waitingWriters + if not b then 1 else 0)
        readMVar readLock
        go True
      (WriteLocked waitingWriters) -> do
        -- There is an active writer. Queue ourselves up so that readers are
        -- blocked from acquiring the lock and wait until the current writer is done.
        putMVar state $! WriteLocked (waitingWriters + if not b then 1 else 0)
        readMVar writeLock
        go True


releaseWrite :: RWLock -> IO ()
releaseWrite RWLock{..} = mask_ $
   takeMVar state >>= \case
    (WriteLocked waitingWriters) -> do
      putMVar writeLock ()
      putMVar state (Free waitingWriters)
    lockState -> do
      putMVar state lockState
      error $ "releaseWrite: attempting to release while in state: " ++ show lockState


releaseRead :: RWLock -> IO ()
releaseRead RWLock{..} = mask_ $
  takeMVar state >>= \case
    (ReadLocked 1 waitingWriters) -> do
      putMVar readLock ()
      putMVar state (Free waitingWriters)
    (ReadLocked n waitingWriters) -> putMVar state $! ReadLocked (n - 1) waitingWriters
    lockState -> do
      putMVar state lockState
      error $ "releaseRead: attempting to release read when in state: " ++ show lockState

-- |Acquire the write lock and execute the action. The lock will be released
-- even if the action raises an exception.
withWriteLock :: RWLock -> IO a -> IO a
withWriteLock ls = bracket_ (acquireWrite ls) (releaseWrite ls)

-- |Acquire the read lock and execute the action. The lock will be released even
-- if the action raises an exception.
withReadLock :: RWLock -> IO a -> IO a
withReadLock ls = bracket_ (acquireRead ls) (releaseRead ls)

-- |LMDB database environment with a reader-writer lock. The reader writer lock
-- is used to make sure that modifications of the environment are done when
-- there are no active transactions. The intended way to use this is to acquire
-- a read lock for each normal transaction (e.g., read, write), and acquire a
-- write lock when the environment needs to be modified, e.g., resized.
data StoreEnv = StoreEnv {
  -- |The LMDB environment.
  _seEnv :: !MDB_env,
  -- |Lock to quard access to the environment. When resizing the environment
  -- we must ensure that there are no outstanding transactions.
  _seEnvLock :: !RWLock
}
makeLenses ''StoreEnv

makeStoreEnv :: IO StoreEnv
makeStoreEnv = do
  _seEnv <- mdb_env_create
  _seEnvLock <- initializeLock
  return StoreEnv{..}

-- |Acquire exclusive access to the LMDB environment and perform the given action.
withWriteStoreEnv :: StoreEnv -> (MDB_env -> IO a) -> IO a
withWriteStoreEnv env f = withWriteLock (env ^. seEnvLock) (f (env ^. seEnv))

-- |A type-safe wrapper for an LMDB database.  Typically, this can be
-- implemented with a @newtype@ wrapper around 'MDB_dbi'' and defining
-- the associated types 'DBKey' and 'DBValue' to be 'Serializable' types
-- representing the keys and values of the database. For example:
--
-- > newtype IntStringDB = IntStringDB MDB_dbi'
-- > instance MDBDatabase IntStringDB where
-- >    type DBKey IntStringDB = Int
-- >    type DBValue IntStringDB = String
--
-- Overriding the encoding and decoding functions for keys and values
-- can be used to improve performance or provide more specialized
-- encodings.
class MDBDatabase db where
  -- |Type of keys of the database.
  type DBKey db
  -- |Type of values of the database.
  type DBValue db
  -- |Obtain the database handle.
  mdbDatabase :: db -> MDB_dbi'
  default mdbDatabase :: (Coercible db MDB_dbi') => db -> MDB_dbi'
  mdbDatabase = coerce
  -- |Encode a key as a strict byte string. (Strict because it must be short.)
  encodeKey :: Proxy db -> DBKey db -> ByteString
  default encodeKey :: (S.Serialize (DBKey db)) => Proxy db -> DBKey db -> ByteString
  encodeKey _ = S.encode
  -- |Decode a key. The result should not retain the pointer.
  decodeKey :: Proxy db -> MDB_val -> IO (Either String (DBKey db))
  default decodeKey :: (S.Serialize (DBKey db)) => Proxy db -> MDB_val -> IO (Either String (DBKey db))
  decodeKey _ k = S.decode <$> byteStringFromMDB_val k
  -- |Encode a value as a lazy byte string. (Lazy because it may be more memory efficient.)
  encodeValue :: Proxy db -> DBValue db -> LBS.ByteString
  default encodeValue :: (S.Serialize (DBValue db)) => Proxy db -> DBValue db -> LBS.ByteString
  encodeValue _ = S.encodeLazy
  -- |Decode a value. The result should not retain the pointer.
  decodeValue :: Proxy db -> MDB_val -> IO (Either String (DBValue db))
  default decodeValue :: (S.Serialize (DBValue db)) => Proxy db -> MDB_val -> IO (Either String (DBValue db))
  decodeValue _ v = S.decode <$> byteStringFromMDB_val v

-- |Run a transaction in an LMDB environment. The second argument specifies if
-- the transaction is read-only. This will acquire a read lock so the given IO
-- action must not contain a transaction, or acquire a read or write lock
-- itself. Doing so is likely to lead to a deadlock if other threads have access
-- to the same environment.
transaction :: StoreEnv -> Bool -> (MDB_txn -> IO a) -> IO a
transaction se readOnly tx
  = threadRun $ mask $ \unmask ->
      withReadLock lock $ do
        txn <- mdb_txn_begin env Nothing readOnly
        res <- unmask (tx txn) `onException` mdb_txn_abort txn
        mdb_txn_commit txn
        return res
  where
    env = se ^. seEnv
    lock = se ^. seEnvLock
    threadRun
        | readOnly = id
        | otherwise = runInBoundThread

-- |Use a 'ByteString' as an 'MDB_val'.  This uses 'BS.unsafeUseAsCStringLen',
-- which means some caveats apply.  If the string is zero-length, the pointer
-- is not required to be valid (and may be null).  The data at the pointer
-- should also not be overwritten as this will unsafely modify the ByteString.
withMDB_val :: ByteString -> (MDB_val -> IO a) -> IO a
withMDB_val bs a = BS.unsafeUseAsCStringLen bs $ \(ptr, plen) -> a $ MDB_val (fromIntegral plen) (coerce ptr)

-- |Create a 'ByteString' from an 'MDB_val'.  This creates a copy.
byteStringFromMDB_val :: MDB_val -> IO ByteString
byteStringFromMDB_val (MDB_val len ptr) = packCStringLen (coerce ptr, fromIntegral len)

-- |Write a lazy 'LBS.ByteString' into an 'MDB_val'.
-- The destination must have the same size as the source.
writeMDB_val :: LBS.ByteString -> MDB_val -> IO ()
writeMDB_val lbs v = assert (LBS.length lbs == fromIntegral (mv_size v)) $ do
  let f ptr chunk =
        BS.unsafeUseAsCStringLen chunk $ \(cptr, clen) -> do
          copyBytes ptr cptr clen
          return $ plusPtr ptr clen
  foldM_ f (coerce $ mv_data v) (LBS.toChunks lbs)

-- |A 'PrimitiveCursor' provides a wrapper for an 'MDB_cursor'', together
-- with two pointers for storing references to keys and values.
data PrimitiveCursor = PrimitiveCursor {
  pcCursor :: !MDB_cursor',
  pcKeyPtr :: !(Ptr MDB_val),
  pcValPtr :: !(Ptr MDB_val)
}

-- |Open a cursor on a database. This also allocates pointers for holding keys
-- and values retrieved via the cursor.  After the continuation is run, the
-- cursor is closed and pointers deallocated.  The 'PrimitiveCursor' should
-- thus not be retained or used after the continuation has returned.
withPrimitiveCursor :: MDB_txn -> MDB_dbi' -> (PrimitiveCursor -> IO a) -> IO a
withPrimitiveCursor txn db op =
    bracket (mdb_cursor_open' txn db) mdb_cursor_close' $ \pcCursor ->
    bracket malloc free $ \pcKeyPtr ->
    bracket malloc free $ \pcValPtr -> op PrimitiveCursor{..}

-- |Operations on cursors
data CursorMove
  = CursorCurrent
  -- ^Stay at the current position
  | CursorFirst
  -- ^Move to the first key
  | CursorLast
  -- ^Move to the last key
  | CursorNext
  -- ^Move to the next key
  | CursorPrevious
  -- ^Move to the previous key

-- |Move a cursor and read the key and value at the new location.
getPrimitiveCursor :: CursorMove -> PrimitiveCursor -> IO (Maybe (MDB_val, MDB_val))
getPrimitiveCursor movement PrimitiveCursor{..} = do
    res <- mdb_cursor_get' moveOp pcCursor pcKeyPtr pcValPtr
    if res then do
      key <- peek pcKeyPtr
      val <- peek pcValPtr
      return $ Just (key, val)
    else
      return Nothing
  where
    moveOp = case movement of
      CursorCurrent -> MDB_GET_CURRENT
      CursorFirst -> MDB_FIRST
      CursorLast -> MDB_LAST
      CursorNext -> MDB_NEXT
      CursorPrevious -> MDB_PREV

-- |Move a cursor to a specified key.
movePrimitiveCursor :: MDB_val -> PrimitiveCursor -> IO (Maybe MDB_val)
movePrimitiveCursor target PrimitiveCursor{..} = do
    poke pcKeyPtr target
    res <- mdb_cursor_get' MDB_SET_KEY pcCursor pcKeyPtr pcValPtr
    if res then
      Just <$> peek pcValPtr
    else
      return Nothing

-- |Traverse a database, using a cursor internally.
-- 'MDB_val' values must not be retained as they will be reused.
traverseTable :: MDB_txn -> MDB_dbi' -> (a -> MDB_val -> MDB_val -> IO a) -> a -> IO a
traverseTable txn db step start =
    withPrimitiveCursor txn db $ \cursor -> do
      let
          loop Nothing cur = return cur
          loop (Just (key, val)) cur = do
            nxt <- step cur key val
            nxtRes <- getPrimitiveCursor CursorNext cursor
            loop nxtRes nxt
      fstRes <- getPrimitiveCursor CursorFirst cursor
      loop fstRes start

-- |A type-safe cursor over a database.
newtype Cursor db = Cursor PrimitiveCursor

-- |Open a cursor on a database.  After the continuation returns, the cursor is
-- freed and should not be retained or used.
withCursor :: (MDBDatabase db) => MDB_txn -> db -> (Cursor db -> IO a) -> IO a
withCursor txn db op = withPrimitiveCursor txn (mdbDatabase db) (op . Cursor)

-- |Move a cursor and read the key and value at the new location.
getCursor :: forall db. (MDBDatabase db) => CursorMove -> Cursor db -> IO (Maybe (Either String (DBKey db, DBValue db)))
getCursor movement (Cursor pc) = getPrimitiveCursor movement pc >>= mapM decodeKV
  where
    decodeKV (keyv, valv) = runExceptT $ do
      key <- ExceptT $ decodeKey prox keyv
      val <- ExceptT $ decodeValue prox valv
      return (key, val)
    prox :: Proxy db
    prox = Proxy

-- |Load all key value pairs from a database. Raises an error on a serialization failure.
loadAll :: forall db. (MDBDatabase db) => MDB_txn -> db -> IO [(DBKey db, DBValue db)]
loadAll txn db = withCursor txn db $ \cursor -> do
        let trav l (Just (Right kv)) = trav (kv : l) =<< getCursor CursorPrevious cursor
            trav l Nothing = return l
            trav _ (Just (Left e)) = error e
        trav [] =<< getCursor CursorLast cursor

-- |Store a record. Do not replace an existing record at the same key.
storeRecord :: forall db. (MDBDatabase db)
  => MDB_txn
  -- ^Transaction
  -> db
  -- ^Table
  -> DBKey db
  -- ^Key
  -> DBValue db
  -- ^Value
   -> IO ()
storeRecord txn dbi key val = withMDB_val (encodeKey prox key) $ \keyv -> do
    let encVal = encodeValue prox val
    res <- tryJust isKeyExist $ mdb_reserve' writeFlags txn (mdbDatabase dbi) keyv (fromIntegral $ LBS.length encVal)
    case res of
      Left _ -> return () -- Key exists, so nothing to do
      Right valv -> writeMDB_val encVal valv
  where
    prox :: Proxy db
    prox = Proxy
    writeFlags = compileWriteFlags [MDB_NOOVERWRITE]
    isKeyExist LMDB_Error{e_code = Right MDB_KEYEXIST} = Just ()
    isKeyExist _ = Nothing

-- |Store a record. Replace any existing record at the same key.
storeReplaceRecord :: forall db. (MDBDatabase db)
  => MDB_txn
  -- ^Transaction
  -> db
  -- ^Table
  -> DBKey db
  -- ^Key
  -> DBValue db
  -- ^Value
   -> IO ()
storeReplaceRecord txn dbi key val = withMDB_val (encodeKey prox key) $ \keyv -> do
    let encVal = encodeValue prox val
    valv <- mdb_reserve' writeFlags txn (mdbDatabase dbi) keyv (fromIntegral $ LBS.length encVal)
    writeMDB_val encVal valv
  where
    prox :: Proxy db
    prox = Proxy
    writeFlags = compileWriteFlags []

-- |Load a record at the specified key.
loadRecord :: forall db. (MDBDatabase db)
  => MDB_txn
  -- ^Transaction
  -> db
  -- ^Table
  -> DBKey db
  -- ^Key
  -> IO (Maybe (DBValue db))
loadRecord txn dbi key = do
    mval <- withMDB_val (encodeKey prox key) $ mdb_get' txn (mdbDatabase dbi)
    case mval of
      Nothing -> return Nothing
      Just bval -> decodeValue prox bval >>= \case
        Left _ -> return Nothing
        Right val -> return (Just val)
  where
    prox :: Proxy db
    prox = Proxy

-- |Determine if a value exists in the database with the specified key.
isRecordPresent :: forall db. (MDBDatabase db)
  => MDB_txn
  -- ^Transaction
  -> db
  -- ^Table
  -> DBKey db
  -- ^Key
  -> IO Bool
isRecordPresent txn dbi key =
    isJust <$> withMDB_val (encodeKey prox key) (mdb_get' txn (mdbDatabase dbi))
  where
    prox :: Proxy db
    prox = Proxy

-- |Determine the number of entries in a database.
databaseSize :: forall db. (MDBDatabase db)
  => MDB_txn
  -- ^Transaction
  -> db
  -- ^Table
  -> IO Word64
databaseSize txn dbi = fromIntegral . ms_entries <$> mdb_stat' txn (mdbDatabase dbi)
