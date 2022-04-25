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
module Concordium.GlobalState.LMDB.Helpers (
  -- * Database environment.
  StoreEnv,
  makeStoreEnv,
  withWriteStoreEnv,
  seEnv,

  -- * Database queries and updates.
  MDBDatabase(..),
  transaction,
  isRecordPresent,
  storeRecord,
  storeReplaceRecord,
  storeReplaceBytes,
  loadRecord,
  databaseSize,

  -- * Traversing the database.
  CursorMove(..),
  withCursor,
  getCursor,
  getPrimitiveCursor,
  movePrimitiveCursor,
  withPrimitiveCursor,
  PrimitiveCursor,
  traverseTable,
  loadAll,

  -- * Low level operations.
  byteStringFromMDB_val
                                           )
where

import Control.Concurrent (runInBoundThread, yield)
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

-- |State of a reader-writer lock.
data RWState =
   -- |Nobody has acquired the lock.
  Free {
      -- |The lock is not acquired, but there might be pending writers that want to acquire it.
      waitingWriters :: !Word64
      }
  -- |There is at least one active reader.
  | ReadLocked {
      -- |The number of readers that are currently active.
      readers :: !Word64,
      -- |The number of pending writers.
      waitingWriters :: !Word64
      }
  -- |The lock is acquired by a single writer.
  | WriteLocked {
      -- |The number of writers that are pending (that is, currently blocked on this lock).
      waitingWriters :: !Word64
      }
    deriving(Show)

-- |A reader-writer lock that strongly prefer writers. More precisely this means the following
-- - readers and writers are mutually exclusive
-- - multiple readers may hold the lock at the same time if there is no writer
-- - at most one writer may hold the lock
--
-- If a writer tries to acquire a lock it will either
-- - succeed if there are no current readers or writers
-- - block after recording the intent to lock. While there are pending writers no new readers can acquire the lock.
--
-- If multiple writers are blocking on the lock they will be served in an
-- unspecified order and in principle it is possible that with heavy write
-- contention some writers would be starved. This is not the case for the
-- use-case we have.
--
-- Let ⊤ mean that the MVar is full and ⊥ that it is empty. The fields of the lock satisfy the following
-- properties.
-- - there are exactly waitingWriters threads blocking on acquireWrite
-- - rwlState == Free if and only if rwlReadLock == ⊤ and rwlWriteLock == ⊤
-- - rwlReadLock == ⊥ if and only if there is an active reader.
-- - rwlWriteLock == ⊥ if and only if there is an active writer.
--
-- Transitions between states are governed by the following transition system
-- where AW/RW and AR/RR mean acquire write, release write and acquire read,
-- release read, respectively. The WR and WW mean that the thread that
-- executed the transition is blocked waiting for rwlReadLock and rwlWriteLock MVar to be full.
-- (Free 0, ⊤, ⊤) -AR-> (ReadLocked 1 0, ⊥, ⊤)
-- (Free (n+1), ⊤, ⊤) -AR-> (Free (n+1), ⊤, ⊤)
-- (Free 0, ⊤, ⊤) -AW-> (WriteLocked 0, ⊤, ⊥)
-- (Free (n+1), ⊤, ⊤) -AW-> (WriteLocked n, ⊤, ⊥)
--
-- (ReadLocked n 0, ⊥, ⊤) -AR-> (ReadLocked (n+1) 0, ⊥, ⊤)
-- (ReadLocked n (m+1), ⊥, ⊤) -AR-> (ReadLocked n (m+1), ⊥, ⊤), WR
-- (ReadLocked n m, ⊥, ⊤) -AW-> (ReadLocked n (m+1), ⊥, ⊤), WW
-- (ReadLocked 1 m, ⊥, ⊤) -RR-> (Free m, ⊤, ⊤)
-- (ReadLocked (n+1) m, ⊥, ⊤) -RR-> (ReadLocked n m, ⊥, ⊤)
--
-- (WriteLocked n, ⊤, ⊥) -AR-> (WriteLocked n, ⊤, ⊥)
-- (WriteLocked n, ⊤, ⊥) -AW-> (WriteLocked (n+1), ⊤, ⊥), WR
-- (WriteLocked n, ⊤, ⊥) -RW-> (Free n, ⊤, ⊤), WW
--
-- No other state should be reachable.
--
-- Additionally, rwlReadLock and rwlWriteLock can only be modified while the
-- rwlState MVar is held.
data RWLock = RWLock {
  -- |The state the lock is currently in.
  rwlState :: !(MVar RWState),
  -- |An MVar used to signal threads that are waiting for all active readers to
  -- wake up. This is empty when there is at least one active reader and full
  -- otherwise.
  rwlReadLock :: !(MVar ()),
  -- |An MVar used to signal waiting readers and writers to wake up. This is
  -- empty when there is an active writer, and full otherwise. Readers wait on
  -- this MVar when there is an active writer.
  rwlWriteLock :: !(MVar ())
  }

-- |Initialize a lock in the unlocked state.
initializeLock :: IO RWLock
initializeLock = do
  rwlState <- newMVar (Free 0)
  rwlReadLock <- newMVar ()
  rwlWriteLock <- newMVar ()
  return RWLock{..}

-- |Acquire a read lock. This will block until there are no pending writers
-- waiting to acquire the lock.
acquireRead :: RWLock -> IO ()
acquireRead RWLock{..} = mask_ go
  where
    go = takeMVar rwlState >>= \case
      st@(Free waitingWriters)
        | waitingWriters == 0 -> do
            -- the lock is free and there are no waiting writers. Acquire a read lock.
            takeMVar rwlReadLock
            putMVar rwlState (ReadLocked 1 0)
        | otherwise -> do
            -- the lock is free, but there are waiting writers. We do nothing and try again.
            -- Due to fairness of MVars next time another thread will make progress
            -- so we are going to end up after a finite number of iterations, in a WriteLocked state.
            putMVar rwlState st
            -- Since this branch seems to be compiled into a loop without
            -- allocations by GHC with -O2 we need to explicitly yield to allow others
            -- to make progress. Otherwise with sufficient contention this loop ends up
            -- starving other threads since they are never scheduled. This then also means
            -- the loop never terminates since no other thread transitions from the Free
            -- to WriteLocked state.
            yield
            go
      st@(ReadLocked n waitingWriters)
          | waitingWriters == 0 ->
            -- No waiting writers, add another reader.
            putMVar rwlState $! ReadLocked (n + 1) 0
          | otherwise -> do
              -- Some readers hold the lock, but there are waiting writers.
              -- We do nothing and wait until there are no more readers and attempt again.
              -- At that point we are likely to end up in WriteLocked state.
              putMVar rwlState st
              readMVar rwlReadLock
              go
      lockState@(WriteLocked _) -> do
        -- There is an active writer. Do nothing and wait until the writer is done.
        putMVar rwlState lockState
        readMVar rwlWriteLock
        go

-- |Acquire a write lock. This will block when there are active readers or
-- writers. When this is operation is blocked it also blocks new readers from
-- acquiring the lock.
acquireWrite :: RWLock -> IO ()
acquireWrite RWLock{..} = mask_ $ go False
  where
    -- the boolean flag indicates whether this is a first iteration of the loop (False) or not (True)
    go alreadyWaiting = takeMVar rwlState >>= \case
      (Free waitingWriters) -> do
        -- The lock is free, take it.
        takeMVar rwlWriteLock
        putMVar rwlState $! WriteLocked (waitingWriters - if alreadyWaiting then 1 else 0)
      (ReadLocked n waitingWriters) -> do
        -- There are active readers. Queue ourselves up and wait until all existing readers
        -- are done. This will block all subsequent readers from acquiring the lock.
        putMVar rwlState $! ReadLocked n (waitingWriters + if alreadyWaiting then 0 else 1)
        readMVar rwlReadLock
        go True
      (WriteLocked waitingWriters) -> do
        -- There is an active writer. Queue ourselves up so that readers are
        -- blocked from acquiring the lock and wait until the current writer is done.
        putMVar rwlState $! WriteLocked (waitingWriters + if alreadyWaiting then 0 else 1)
        readMVar rwlWriteLock
        go True

-- |Release the write lock. The lock is assumed to be in write state, otherwise
-- this function will raise an exception.
releaseWrite :: RWLock -> IO ()
releaseWrite RWLock{..} = mask_ $
   takeMVar rwlState >>= \case
    (WriteLocked waitingWriters) -> do
      putMVar rwlWriteLock ()
      putMVar rwlState (Free waitingWriters)
    lockState -> do
      putMVar rwlState lockState
      error $ "releaseWrite: attempting to release while in state: " ++ show lockState


-- |Release the read lock. The lock is assumed to be in read state, otherwise
-- this function will raise an exception. Note that since multiple readers may
-- acquire the read lock at the same time this either decrements the read count
-- and leaves the lock in read state, or unlocks it if called when there is only
-- a single active reader.
releaseRead :: RWLock -> IO ()
releaseRead RWLock{..} = mask_ $
  takeMVar rwlState >>= \case
    (ReadLocked 1 waitingWriters) -> do
      putMVar rwlReadLock ()
      putMVar rwlState (Free waitingWriters)
    (ReadLocked n waitingWriters) -> putMVar rwlState $! ReadLocked (n - 1) waitingWriters
    lockState -> do
      putMVar rwlState lockState
      error $ "releaseRead: attempting to release read when in state: " ++ show lockState

-- |Acquire the write lock and execute the action. The lock will be released
-- even if the action raises an exception. See 'acquireWrite' for more details.
withWriteLock :: RWLock -> IO a -> IO a
withWriteLock ls = bracket_ (acquireWrite ls) (releaseWrite ls)

-- |Acquire the read lock and execute the action. The lock will be released even
-- if the action raises an exception. See 'acquireRead' for more details.
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

-- |Construct a new LMDB environment with associated locks that protect its use.
makeStoreEnv :: IO StoreEnv
makeStoreEnv = do
  _seEnv <- mdb_env_create
  _seEnvLock <- initializeLock
  return StoreEnv{..}

-- |Acquire exclusive access to the LMDB environment and perform the given action.
-- The IO action should not leak the 'MDB_env'.
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
storeReplaceRecord txn dbi key val = do
    let encVal = encodeValue prox val
    storeReplaceBytes txn dbi key encVal
  where
    prox :: Proxy db
    prox = Proxy

-- |Store a serialized record. Replace any existing record at the same key.
storeReplaceBytes :: forall db. (MDBDatabase db)
  => MDB_txn
  -- ^Transaction
  -> db
  -- ^Table
  -> DBKey db
  -- ^Key
  -> LBS.ByteString
  -- ^Value
   -> IO ()
storeReplaceBytes txn dbi key encVal = withMDB_val (encodeKey prox key) $ \keyv -> do
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
