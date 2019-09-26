
{-# LANGUAGE RecordWildCards, MultiParamTypeClasses, FunctionalDependencies, TypeFamilies, FlexibleInstances, QuantifiedConstraints,
    GeneralizedNewtypeDeriving, StandaloneDeriving, UndecidableInstances, DefaultSignatures, DeriveFunctor, ConstraintKinds, RankNTypes,
    ScopedTypeVariables, TupleSections, DeriveFoldable, DeriveTraversable, DerivingStrategies, FlexibleContexts #-}
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
import Data.Proxy
import GHC.Stack

import Concordium.GlobalState.Persistent.MonadicRecursive

newtype BlobRef a = BlobRef Word64
    deriving (Eq, Ord, Serialize)

instance Show (BlobRef a) where
    show (BlobRef v) = '@' : show v

nullRef :: BlobRef a
nullRef = BlobRef maxBound

data BlobStore = BlobStore {
    blobStoreFile :: !(MVar Handle),
    blobStoreFilePath :: !FilePath
}

class HasBlobStore a where
    blobStore :: a -> BlobStore

instance HasBlobStore BlobStore where
    blobStore = id

createTempBlobStore :: IO BlobStore
createTempBlobStore = do
    tempDir <- getTemporaryDirectory
    (tempFP, h) <- openBinaryTempFile tempDir "blb.dat"
    mv <- newMVar h
    return $! BlobStore mv tempFP

destroyTempBlobStore :: BlobStore -> IO ()
destroyTempBlobStore BlobStore{..} = do
    h <- takeMVar blobStoreFile
    hClose h
    removeFile blobStoreFilePath

runBlobStoreTemp :: FilePath -> ReaderT BlobStore IO a -> IO a
runBlobStoreTemp fp a = bracket openf closef usef
    where 
        openf = openBinaryTempFile fp "blb.dat"
        closef (tempFP, h) = do
            hClose h
            removeFile tempFP
        usef (fp, h) = do
            mv <- newMVar h
            res <- runReaderT a (BlobStore mv fp)
            _ <- takeMVar mv
            return res

readBlobBS :: BlobStore -> BlobRef a -> IO BS.ByteString
readBlobBS BlobStore{..} (BlobRef offset) = bracketOnError (takeMVar blobStoreFile) (tryPutMVar blobStoreFile) $ \h -> do
        hSeek h AbsoluteSeek (fromIntegral offset)
        esize <- decode <$> BS.hGet h 8
        case esize :: Either String Word64 of
            Left e -> error e
            Right size -> do
                bs <- BS.hGet h (fromIntegral size)
                putMVar blobStoreFile h
                return bs

readBlob :: (Serialize a) => BlobStore -> BlobRef a -> IO a
readBlob bstore ref = do
        bs <- readBlobBS bstore ref
        case decode bs of
            Left e -> error e
            Right v -> return v

writeBlobBS ::BlobStore -> BS.ByteString -> IO (BlobRef a)
writeBlobBS BlobStore{..} bs = bracketOnError (takeMVar blobStoreFile) (tryPutMVar blobStoreFile) $ \h -> do
        hSeek h SeekFromEnd 0
        offset <- fromIntegral <$> hTell h
        BS.hPut h size
        BS.hPut h bs
        putMVar blobStoreFile h
        return (BlobRef offset)
    where
        size = encode (fromIntegral (BS.length bs) :: Word64)

writeBlob :: (Serialize a) => BlobStore -> a -> IO (BlobRef a)
writeBlob bstore v = writeBlobBS bstore (encode v)

class (Monad m, forall a. Serialize (ref a)) => MonadBlobStore m ref where
    storeRaw :: BS.ByteString -> m (ref a)
    loadRaw :: ref a -> m BS.ByteString
    storeBlob :: (Serialize a) => a -> m (ref a)
    storeBlob = storeRaw . encode
    loadBlob :: (Serialize a) => ref a -> m a
    loadBlob r = do
        bs <- loadRaw r
        case decode bs of
            Left e -> error e
            Right v -> return v

instance (MonadIO m, MonadReader r m, HasBlobStore r) => MonadBlobStore m BlobRef where
    storeRaw b = do
        bs <- blobStore <$> ask
        liftIO $ writeBlobBS bs b
    loadRaw r = do
        bs <- blobStore <$> ask
        liftIO $ readBlobBS bs r
{-
class (MonadBlobStore m ref) => Blobbable m ref a where
    store :: a -> m (ref a)
    default store :: (Serialize a) => a -> m (ref a)
    store = storeBlob
    load :: ref a -> m a
    default load :: (Serialize a) => ref a -> m a
    load = loadBlob
-}

class (MonadBlobStore m ref) => BlobStorable m ref a where
    store :: Proxy ref -> a -> m Put
    default store :: (Serialize a) => Proxy ref -> a -> m Put
    store _ = pure . put
    load :: Proxy ref -> Get (m a)
    default load :: (Serialize a) => Proxy ref -> Get (m a)
    load _ = pure <$> get
    storeUpdate :: Proxy ref -> a -> m (Put, a)
    storeUpdate p v = (,v) <$> store p v

    storeRef :: a -> m (ref a)
    storeRef v = do
        p <- runPut <$> store (Proxy :: Proxy ref) v
        storeRaw p
    storeUpdateRef :: a -> m (ref a, a)
    storeUpdateRef v = do
        (p, v') <- storeUpdate (Proxy :: Proxy ref) v
        (, v') <$> storeRaw (runPut p)
    loadRef :: (HasCallStack) => (ref a) -> m a
    loadRef ref = do
        bs <- loadRaw ref
        case runGet (load (Proxy :: Proxy ref)) bs of
            Left e -> error (e ++ " :: " ++ show bs)
            Right mv -> mv

newtype SerializeStorable v = SerStore v
    deriving newtype (Eq, Ord, Show, Serialize)

instance (Serialize v, MonadBlobStore m ref) => BlobStorable m ref (SerializeStorable v)

data Nullable v = Null | Some !v
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Serialize (Nullable (BlobRef a)) where
    put Null = put nullRef
    put (Some v) = put v
    get = do
        r <- get
        return $! if r == nullRef then Null else Some r

instance (MonadBlobStore m BlobRef) => BlobStorable m BlobRef (BlobRef a)
instance (MonadBlobStore m BlobRef) => BlobStorable m BlobRef (Nullable (BlobRef a))

data CachedRef a
    = CRBlobbed {crRef :: !(BlobRef a)}
    | CRCached {crRef :: !(BlobRef a), crValue :: !a}

instance (MonadBlobStore m BlobRef) => BlobStorable m BlobRef (CachedRef a) where
    store p v = store p (crRef v)
    load p = fmap CRBlobbed <$> load p

instance (MonadBlobStore m BlobRef) => BlobStorable m BlobRef (Nullable (CachedRef a)) where
    store p v = store p (crRef <$> v)
    load p = fmap (fmap CRBlobbed) <$> load p

data BufferedRef a
    = BRBlobbed {brRef :: !(BlobRef a)}
    | BRCached {brRef :: !(BlobRef a), brValue :: !a}
    | BRMemory {brValue :: !a}

instance Show a => Show (BufferedRef a) where
    show (BRBlobbed r) = show r
    show (BRCached r v) = "{" ++ show v ++ "}" ++ show r
    show (BRMemory v) = "{" ++ show v ++ "}"

instance (BlobStorable m BlobRef a) => BlobStorable m BlobRef (BufferedRef a) where
    store p (BRBlobbed r) = store p r
    store p (BRCached r _) = store p r
    store p (BRMemory v) = do
        (r :: BlobRef a) <- storeRef v
        store p r
    load p = fmap BRBlobbed <$> load p
    storeUpdate p (BRMemory v) = do
        (r :: BlobRef a, v') <- storeUpdateRef v
        (,BRCached r v') <$> store p r
    storeUpdate p x = (,x) <$> store p x

instance (BlobStorable m BlobRef a) => BlobStorable m BlobRef (Nullable (BufferedRef a)) where
    store _ Null = return $ put nullRef
    store p (Some v) = store p v
    load p = do
        r <- get
        if r == nullRef then
            return (pure Null)
        else
            fmap Some <$> load p
    storeUpdate _ n@Null = return (put nullRef, n)
    storeUpdate p (Some v) = do
        (r, v') <- storeUpdate p v
        return (r, Some v')

loadBufferedRef :: (HasCallStack, BlobStorable m BlobRef a) => BufferedRef a -> m a
loadBufferedRef (BRBlobbed ref) = loadRef ref
loadBufferedRef (BRCached _ v) = return v
loadBufferedRef (BRMemory v) = return v

{-
class RefStorable ref m x where
    makeRef :: (forall a. ref a -> m Put) -> x -> m (ref x)
    loadRef :: (forall a. Get (m a)) -> m (ref x) -> x
-}

cachedToBlob :: CachedRef a -> BlobRef a
cachedToBlob = crRef

blobToCached :: BlobRef a -> CachedRef a
blobToCached = CRBlobbed

bufferedToCached' :: (BlobStorable m BlobRef a) => BufferedRef a -> m (CachedRef a)
bufferedToCached' (BRBlobbed r) = return $ CRBlobbed r
bufferedToCached' (BRCached r v) = return $ CRCached r v
bufferedToCached' (BRMemory v) = do
        (r, v') <- storeUpdateRef v
        return $ CRCached r v'

flushBuffered :: (BlobStorable m BlobRef a) => BufferedRef a -> m (BufferedRef a)
flushBuffered (BRMemory v) = do
        (r, v') <- storeUpdateRef v
        return $ BRCached r v'
flushBuffered b = return b

uncacheBuffered :: (BlobStorable m BlobRef a) => BufferedRef a -> m (BufferedRef a)
uncacheBuffered (BRMemory v) = do
        r <- storeRef v
        return $ BRBlobbed r
uncacheBuffered (BRCached r _) = return $ BRBlobbed r
uncacheBuffered b = return b


newtype Blobbed ref f = Blobbed {unblobbed :: ref (f (Blobbed ref f)) }
    
deriving instance (forall a. Serialize (ref a)) => Serialize (Blobbed ref f)

instance (MonadBlobStore m ref) => BlobStorable m ref (Blobbed ref f)

instance (forall a. Serialize (Nullable (ref a))) => Serialize (Nullable (Blobbed ref f)) where
    put = put . fmap unblobbed
    get = fmap Blobbed <$> get

instance (MonadBlobStore m ref, forall a. Serialize (Nullable (ref a))) => BlobStorable m ref (Nullable (Blobbed ref f)) where

type instance Base (Blobbed ref f) = f

instance (MonadBlobStore m ref, BlobStorable m ref (f (Blobbed ref f))) => MRecursive m (Blobbed ref f) where
    mproject (Blobbed r) = loadRef r

instance (MonadBlobStore m ref, BlobStorable m ref (f (Blobbed ref f))) => MCorecursive m (Blobbed ref f) where
    membed r = Blobbed <$> storeRef r


data CachedBlobbed ref f
    = CBUncached (Blobbed ref f)
    | CBCached (Blobbed ref f) (f (CachedBlobbed ref f))

cachedBlob :: CachedBlobbed ref f -> Blobbed ref f
cachedBlob (CBUncached r) = r
cachedBlob (CBCached r _) = r

type instance Base (CachedBlobbed ref f) = f

instance (MonadBlobStore m ref, BlobStorable m ref (f (Blobbed ref f)), Functor f) => MRecursive m (CachedBlobbed ref f) where
    mproject (CBUncached r) = fmap CBUncached <$> mproject r
    mproject (CBCached _ c) = pure c

instance (MonadBlobStore m ref, BlobStorable m ref (f (Blobbed ref f)), Functor f) => MCorecursive m (CachedBlobbed ref f) where
    membed r = do
        b <- membed (fmap cachedBlob r)
        return (CBCached b r)

instance (forall a. Serialize (ref a)) => Serialize (CachedBlobbed ref f) where
    put = put . cachedBlob
    get = CBUncached <$> get

instance (MonadBlobStore m ref) => BlobStorable m ref (CachedBlobbed ref f)




data BufferedBlobbed ref f
    = LBMemory (f (BufferedBlobbed ref f))
    | LBCached (CachedBlobbed ref f)

type instance Base (BufferedBlobbed ref f) = f

instance (MonadBlobStore m ref, BlobStorable m ref (f (Blobbed ref f)), Functor f) => MRecursive m (BufferedBlobbed ref f) where
    mproject (LBMemory r) = pure r
    mproject (LBCached c) = fmap LBCached <$> mproject c
    {-# INLINE mproject #-}

instance Monad m => MCorecursive m (BufferedBlobbed ref f) where
    membed = pure . LBMemory
    {-# INLINE membed #-}

instance (MonadBlobStore m ref, Traversable f, BlobStorable m ref (f (Blobbed ref f))) => BlobStorable m ref (BufferedBlobbed ref f) where
    store p v = fst <$> storeUpdate p v

    storeUpdate p v@(LBCached c) = (, v) <$> store p c
    storeUpdate p v = do
            (pu, v') <- sU v
            return (pu, LBCached v')
        where
            sU :: BufferedBlobbed ref f -> m (Put, CachedBlobbed ref f)
            sU (LBCached c) = storeUpdate p c
            sU (LBMemory t) = do
                t' <- mapM (fmap snd . sU) t
                r <- storeRef (cachedBlob <$> t')
                return (put r, CBCached (Blobbed r) t')
    load _ = return . LBCached <$> get


-- |Flush a 'BufferedBlobbed' to the blob store.
bufferedToCached :: (MonadBlobStore m ref, BlobStorable m ref (f (Blobbed ref f)), Traversable f) => BufferedBlobbed ref f -> m (CachedBlobbed ref f)
bufferedToCached (LBMemory r) = mapM bufferedToCached r >>= membed
bufferedToCached (LBCached c) = return c

class FixShowable fix where
    showFix :: Functor f => (f String -> String) -> fix f -> String

instance (forall a. Show (ref a)) => FixShowable (Blobbed ref) where
    showFix _ (Blobbed r) = show r

instance (forall a. Show (ref a)) => FixShowable (CachedBlobbed ref) where
    showFix sh (CBCached r v) = "{" ++ (sh (showFix sh <$> v)) ++ "}" ++ showFix sh r
    showFix sh (CBUncached r) = showFix sh r

instance (forall a. Show (ref a)) => FixShowable (BufferedBlobbed ref) where
    showFix sh (LBMemory v) = "{" ++ (sh (showFix sh <$> v)) ++ "}"
    showFix sh (LBCached r) = showFix sh r