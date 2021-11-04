{-# LANGUAGE TemplateHaskell #-}
module GlobalStateTests.AccountTrie where

import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.GlobalState.Persistent.AccountTrie as AT
import Data.Serialize
import Data.List (foldl', sort)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Vector.Unboxed as VU
import Control.Monad
import Control.Monad.Trans
import qualified Control.Monad.State.Strict as State
import Lens.Micro.Platform

import Test.QuickCheck
import Test.Hspec

genValidConfig :: Gen (Int, Int, Bool)
genValidConfig = do
  pathLen <- choose (0, 100000)
  childrenLen <- choose (0, 256)
  hasValue <- arbitrary
  return (pathLen, childrenLen, hasValue)

genKey :: Gen AT.Key
genKey = do
  len <- choose (0, 1000)
  VU.replicateM len arbitrary

genInputs :: Gen ([(AT.Key, Word)], [AT.Key])
genInputs = sized $ \n -> do
  len <- choose (0, min n 10000)
  inputs <- replicateM len ((,) <$> genKey <*> arbitrary)
  extraKeysLen <- choose (0, min n 1000)
  extraKeys <- replicateM extraKeysLen genKey
  return (inputs, extraKeys)

constructReference :: [(AT.Key, Word)] -> Map.Map AT.Key Word
constructReference = Map.fromList

constructTrie :: [(AT.Key, Word)] -> AT.PureNode Word
constructTrie = foldl' (\acc (k, v) -> snd $ AT.insertPure k v acc) AT.empty

nodeValueLenSerializationTest :: Word -> Spec
nodeValueLenSerializationTest lvl = it "node value serialization" $ withMaxSuccess (100 ^ lvl) $ property $ do
  forAll genValidConfig $ \cfg@(pathLen, childrenLen, hasValue) -> runGet AT.getNodeValueLen (runPut (AT.putNodeValueLen pathLen childrenLen hasValue)) === Right cfg

insertionTests :: Word -> Spec
insertionTests lvl = it "insert and lookup" $
    withMaxSuccess (10000 * fromIntegral lvl) $ property $
    forAll genInputs $ \(inputs, extraKeys) ->
                         let referenceMap = constructReference inputs
                         in let trie = constructTrie inputs
                            in conjoin (map (\k -> Map.lookup k referenceMap === AT.lookupPure k trie) (map fst inputs ++ extraKeys))

newtype Buffer = Buffer {
  _buffer :: BSL.ByteString
  }
makeLenses ''Buffer

emptyBuffer :: Buffer
emptyBuffer = Buffer BSL.empty

instance MonadIO m => MonadBlobStore (State.StateT Buffer m) where
  storeRaw bs = do
    pos <- BlobRef . fromIntegral <$> use (buffer . to BSL.length)
    let len = runPutLazy (putWord64be (fromIntegral (BS.length bs)))
    buffer %= (`BSL.append` (len `BSL.append` BSL.fromStrict bs))
    return pos
  loadRaw (BlobRef pos) = do
    bs <- use buffer
    case runGetLazy (getByteString . fromIntegral =<< getWord64be) . BSL.drop (fromIntegral pos) $ bs of
      Left err -> error $ "Error reading: " ++ err
      Right x -> return x

  flushStore = return ()

updateM :: AT.Key -> Word -> State.StateT (BlobRef (AT.PersistentNode Word)) (State.StateT Buffer IO) ()
updateM k v = do
  ref <- State.get
  newRef <- lift $ do
    trie <- loadRef ref
    newTrie <- snd <$> AT.insertPersistent k v trie
    storeRef newTrie
  State.put newRef

lookupM :: AT.Key -> State.StateT (BlobRef (AT.PersistentNode Word)) (State.StateT Buffer IO) (Maybe Word)
lookupM k = do
  ref <- State.get
  lift $ do
    trie <- loadRef ref
    AT.lookupPersistent k trie

runUpdates :: [(AT.Key, Word)] -> IO [Maybe Word]
runUpdates inputs = do
  let comp = do
        mapM_ (uncurry updateM) inputs
        mapM (lookupM . fst) inputs
  (initialBlobRef, initialBuffer) <- State.runStateT (storeRef AT.empty) emptyBuffer
  State.evalStateT (State.evalStateT comp initialBlobRef) initialBuffer

makeToList :: [(AT.Key, Word)] -> IO [(AT.Key, Word)]
makeToList inputs = do
  let comp = do
        mapM_ (uncurry updateM) inputs
        ref <- State.get
        lift $ AT.toListPersistent =<< loadRef ref
  (initialBlobRef, initialBuffer) <- State.runStateT (storeRef AT.empty) emptyBuffer
  State.evalStateT (State.evalStateT comp initialBlobRef) initialBuffer


persistentInsertionTests :: Word -> Spec
persistentInsertionTests lvl = it "insert and lookup in the persistent map" $
    withMaxSuccess (10000 * fromIntegral lvl) $ property $
    forAll genInputs $ \(inputs, extraKeys) ->
                         let referenceMap = constructReference inputs
                         in ioProperty $ do
                           results <- runUpdates inputs
                           return $ conjoin (zipWith (\k res -> Map.lookup k referenceMap === res) (map fst inputs ++ extraKeys) results)


persistentToListTests :: Word -> Spec
persistentToListTests lvl = it "to list the persistent map" $
    withMaxSuccess (10000 * fromIntegral lvl) $ property $
    forAll genInputs $ \(inputs, _) ->
                         let referenceMap = constructReference inputs
                         in ioProperty $ do
                           results <- makeToList inputs
                           return $ sort results === Map.toAscList referenceMap

tests :: Word -> Spec
tests lvl = describe "GlobalStateTests.AccountTrie" $ do
              nodeValueLenSerializationTest lvl
              insertionTests lvl
              persistentInsertionTests lvl
              persistentToListTests lvl
