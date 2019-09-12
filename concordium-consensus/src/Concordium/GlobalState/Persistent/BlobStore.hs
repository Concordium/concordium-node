
{-# LANGUAGE RecordWildCards, MultiParamTypeClasses, FunctionalDependencies, TypeFamilies, FlexibleInstances, QuantifiedConstraints,
    GeneralizedNewtypeDeriving, StandaloneDeriving, UndecidableInstances, DefaultSignatures, DeriveFunctor, ConstraintKinds, RankNTypes #-}
{-| 

-}
module Concordium.GlobalState.Persistent.BlobStore where

import Control.Concurrent.MVar
import System.IO
import Data.Serialize
import Data.Word
import qualified Data.ByteString as BS
import Control.Exception
import Data.Functor.Foldable
import Control.Monad.Reader.Class
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.IO.Class
import System.Directory

import Concordium.GlobalState.Persistent.MonadicRecursive

newtype BlobRef a = BlobRef Word64
    deriving (Eq, Ord, Serialize)

nullRef :: BlobRef a
nullRef = BlobRef maxBound

data BlobStore = BlobStore {
    blobStoreFile :: MVar Handle
}

class HasBlobStore a where
    blobStore :: a -> BlobStore

instance HasBlobStore BlobStore where
    blobStore = id

runBlobStoreTemp :: FilePath -> ReaderT BlobStore IO a -> IO a
runBlobStoreTemp fp a = bracket openf closef usef
    where 
        openf = openBinaryTempFile fp "blb.dat"
        closef (tempFP, h) = do
            hClose h
            removeFile tempFP
        usef (_, h) = do
            mv <- newMVar h
            res <- runReaderT a (BlobStore mv)
            _ <- takeMVar mv
            return res

readBlob :: (Serialize a) => BlobStore -> BlobRef a -> IO a
readBlob BlobStore{..} (BlobRef offset) = bracketOnError (takeMVar blobStoreFile) (tryPutMVar blobStoreFile) $ \h -> do
        hSeek h AbsoluteSeek (fromIntegral offset)
        esize <- decode <$> BS.hGet h 8
        case esize :: Either String Word64 of
            Left e -> error e
            Right size -> do
                bs <- BS.hGet h (fromIntegral size)
                putMVar blobStoreFile h
                case decode bs of
                    Left e -> error e
                    Right v -> return v

writeBlob :: (Serialize a) => BlobStore -> a -> IO (BlobRef a)
writeBlob BlobStore{..} v = bracketOnError (takeMVar blobStoreFile) (tryPutMVar blobStoreFile) $ \h -> do
        hSeek h SeekFromEnd 0
        offset <- fromIntegral <$> hTell h
        BS.hPut h size
        BS.hPut h bs
        putMVar blobStoreFile h
        return (BlobRef offset)
    where
        bs = encode v
        size = encode (fromIntegral (BS.length bs) :: Word64)

class (Monad m, forall a. Serialize (ref a), forall a. Serialize (Nullable (ref a))) => MonadBlobStore m ref where
    storeBlob :: (Serialize a) => a -> m (ref a)
    loadBlob :: (Serialize a) => ref a -> m a

instance (MonadIO m, MonadReader r m, HasBlobStore r) => MonadBlobStore m BlobRef where
    storeBlob b = do
        bs <- blobStore <$> ask
        liftIO $ writeBlob bs b
    loadBlob r = do
        bs <- blobStore <$> ask
        liftIO $ readBlob bs r

class (MonadBlobStore m ref) => Blobbable m ref a where
    store :: a -> m (ref a)
    default store :: (Serialize a) => a -> m (ref a)
    store = storeBlob
    load :: ref a -> m a
    default load :: (Serialize a) => ref a -> m a
    load = loadBlob

data Nullable v = Null | Some !v
    deriving (Eq, Ord, Show, Functor)

instance Serialize (Nullable (BlobRef a)) where
    put Null = put nullRef
    put (Some v) = put v
    get = do
        r <- get
        return $! if r == nullRef then Null else Some r

newtype Blobbed ref f = Blobbed {unblobbed :: ref (f (Blobbed ref f)) }
    
deriving instance (forall a. Serialize (ref a)) => Serialize (Blobbed ref f)

instance (forall a. Serialize (Nullable (ref a))) => Serialize (Nullable (Blobbed ref f)) where
    put = put . fmap unblobbed
    get = fmap Blobbed <$> get

type instance Base (Blobbed ref f) = f

type RefSerialize ref f = (forall a. Serialize (f (ref a)))


instance (MonadBlobStore m ref, Serialize (f (Blobbed ref f))) => MRecursive m (Blobbed ref f) where
    mproject (Blobbed r) = loadBlob r

instance (MonadBlobStore m ref, Serialize (f (Blobbed ref f))) => MCorecursive m (Blobbed ref f) where
    membed r = Blobbed <$> storeBlob r

data CachedBlobbed ref f
    = CBUncached (Blobbed ref f)
    | CBCached (Blobbed ref f) (f (CachedBlobbed ref f))

cachedBlob :: CachedBlobbed ref f -> Blobbed ref f
cachedBlob (CBUncached r) = r
cachedBlob (CBCached r _) = r

type instance Base (CachedBlobbed ref f) = f

instance (MonadBlobStore m ref, Serialize (f (Blobbed ref f)), Functor f) => MRecursive m (CachedBlobbed ref f) where
    mproject (CBUncached r) = fmap CBUncached <$> mproject r
    mproject (CBCached _ c) = pure c

instance (MonadBlobStore m ref, Serialize (f (Blobbed ref f)), Functor f) => MCorecursive m (CachedBlobbed ref f) where
    membed r = do
        b <- membed (fmap cachedBlob r)
        return (CBCached b r)

data BufferedBlobbed ref f
    = LBMemory (f (BufferedBlobbed ref f))
    | LBCached (CachedBlobbed ref f)

type instance Base (BufferedBlobbed ref f) = f

instance (MonadBlobStore m ref, Serialize (f (Blobbed ref f)), Functor f) => MRecursive m (BufferedBlobbed ref f) where
    mproject (LBMemory r) = pure r
    mproject (LBCached c) = fmap LBCached <$> mproject c

instance Monad m => MCorecursive m (BufferedBlobbed ref f) where
    membed = pure . LBMemory

-- |Flush a 'BufferedBlobbed' to the blob store.
bufferedToCached :: (MonadBlobStore m ref, Serialize (f (Blobbed ref f)), Traversable f) => BufferedBlobbed ref f -> m (CachedBlobbed ref f)
bufferedToCached (LBMemory r) = mapM bufferedToCached r >>= membed
bufferedToCached (LBCached c) = return c
