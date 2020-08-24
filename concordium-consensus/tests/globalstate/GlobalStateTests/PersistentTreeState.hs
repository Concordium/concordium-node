{-# LANGUAGE DerivingVia, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances, FlexibleInstances, OverloadedStrings, ScopedTypeVariables, TypeFamilies #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module GlobalStateTests.PersistentTreeState where

import qualified Concordium.GlobalState.Persistent.BlockState as PBS
import Concordium.GlobalState.Persistent.TreeState
import Concordium.GlobalState.Classes
import Concordium.Crypto.BlsSignature
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.TreeState
import Concordium.GlobalState.BlockPointer hiding (BlockPointer)
import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.AccountTransactionIndex
import Concordium.GlobalState.Persistent.LMDB
import Concordium.GlobalState.IdentityProviders
import Concordium.GlobalState.AnonymityRevokers
import Concordium.GlobalState.LMDB.Helpers

import Control.Monad.State hiding (state)
import Concordium.GlobalState
import Concordium.GlobalState.DummyData
import Concordium.Crypto.DummyData
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Logger
import Control.Monad.RWS.Strict as RWS hiding (state)
import Data.Time.Clock.POSIX
import Control.Monad.Identity
import Data.Proxy
import Test.Hspec
import Control.Exception
import Lens.Micro.Platform
import Concordium.Crypto.VRF as VRF
import System.FilePath ((</>))
import System.Random
import System.IO.Temp

type GlobalStateIO c g = GlobalStateM NoLogContext c c g g (RWST c () g LogIO)

type TestM = GlobalStateIO PBS.PersistentBlockStateContext (SkovPersistentData () PBS.PersistentBlockState)
type Test = TestM ()

instance HasGlobalStateContext PBS.PersistentBlockStateContext PBS.PersistentBlockStateContext where
  globalStateContext = id

instance HasGlobalState (SkovPersistentData () PBS.PersistentBlockState) (SkovPersistentData () PBS.PersistentBlockState) where
  globalState = id

createGlobalState :: FilePath -> IO (PBS.PersistentBlockStateContext, SkovPersistentData () PBS.PersistentBlockState)
createGlobalState dbDir = do
  now <- utcTimeToTimestamp <$> getCurrentTime
  let
    n = 3
    genesis = makeTestingGenesisData now n 1 1 dummyFinalizationCommitteeMaxSize dummyCryptographicParameters emptyIdentityProviders emptyAnonymityRevokers maxBound dummyAuthorizations dummyChainParameters
    state = basicGenesisState genesis
    config = DTDBConfig (defaultRuntimeParameters { rpTreeStateDir = dbDir, rpBlockStateFile = dbDir </> "blockstate" }) genesis state
  (x, y, NoLogContext) <- runSilentLogger $ initialiseGlobalState config
  return (x, y)

destroyGlobalState :: (PBS.PersistentBlockStateContext, SkovPersistentData () PBS.PersistentBlockState) -> IO ()
destroyGlobalState (c, s) =
  shutdownGlobalState (Proxy :: Proxy DiskTreeDiskBlockConfig) c s NoLogContext

specifyWithGS :: String -> Test -> SpecWith (Arg Expectation)
specifyWithGS s f =
  specify s $
    withTempDirectory "." "test-directory" $
    \dbDir -> bracket (createGlobalState dbDir) destroyGlobalState $
      runSilentLogger . void . uncurry (runRWST (runGlobalStateM $ f))

useI :: MonadState (Identity s) f => Getting b s b -> f b
useI f = (^. f) . runIdentity <$> RWS.get

testFinalizeABlock :: Test
testFinalizeABlock = do
  (genesisBlock, genesisFr) :: (BlockPointerType TestM, FinalizationRecord) <- getLastFinalized
  sk <- liftIO $ generateSecretKey
  state <- blockState genesisBlock
  -- Create the block and finrec
  proof1 <- liftIO $ VRF.prove (fst $ randomKeyPair (mkStdGen 1)) "proof1"
  proof2 <- liftIO $ VRF.prove (fst $ randomKeyPair (mkStdGen 1)) "proof2"
  now <- liftIO $ getCurrentTime
  pb <- makePendingBlock (fst $ randomBlockKeyPair (mkStdGen 1)) 1 (bpHash genesisBlock) 0 proof1 proof2 NoFinalizationData [] now
  now' <- liftIO $ getCurrentTime
  blockPtr :: BlockPointerType TestM <- makeLiveBlock pb genesisBlock genesisBlock state () now' 0
  let frec = FinalizationRecord 1 (bpHash blockPtr) (FinalizationProof ([1], sign "Hello" sk)) 0
  -- Add the finalization to the tree state
  markFinalized (bpHash blockPtr) frec
  addFinalization blockPtr frec

  -- Was updated as the last finalized?
  (b, fr) <- getLastFinalized
  liftIO $ do
    b `shouldBe` blockPtr
    fr `shouldBe` frec

  --- The database should now contain 2 items, check them
  theDB <- use db
  (storedBlocks, finRecs) <- liftIO $ do
    let
        env = theDB ^. storeEnv
        dbB = theDB ^. blockStore
        dbF = theDB ^. finalizationRecordStore
    sB <- transaction env True (flip databaseSize dbB)
    sB `shouldBe` 2
    sF <- transaction env True (flip databaseSize dbF)
    sF `shouldBe` 2
    storedBlocks <- transaction env True (flip loadAll dbB)
    finRecs <- map snd <$> transaction env True (flip loadAll dbF)
    return (storedBlocks, finRecs)
  blocks <- mapM (constructBlock . snd) storedBlocks
  liftIO $ do
    forM_ blocks (`shouldSatisfy` flip elem [blockPtr, genesisBlock])
    forM_ finRecs (`shouldSatisfy` flip elem [frec, genesisFr])

  -- check the blocktable
  bs <- use (blockTable . at (bpHash blockPtr))
  liftIO $ bs `shouldBe` Just (Concordium.GlobalState.Persistent.TreeState.BlockFinalized 1)

  -- check that the parent and last finalized are the proper ones
  parent <- bpParent blockPtr
  liftIO $ parent `shouldBe` genesisBlock
  lfin <- bpLastFinalized blockPtr
  liftIO $ lfin `shouldBe` genesisBlock

  -- add another block with different lfin and parent
  now'' <- liftIO $ getCurrentTime
  pb2 <- makePendingBlock (fst $ randomBlockKeyPair (mkStdGen 1)) 2 (bpHash blockPtr) 0 proof1 proof2 NoFinalizationData [] now''
  now''' <- liftIO $ getCurrentTime
  blockPtr2 :: BlockPointerType TestM <- makeLiveBlock pb2 blockPtr genesisBlock state () now''' 0
  let frec2 = FinalizationRecord 2 (bpHash blockPtr2) (FinalizationProof ([1], sign "Hello" sk)) 0

  -- Add the finalization to the tree state
  markFinalized (bpHash blockPtr2) frec2
  addFinalization blockPtr2 frec2

  --- The database should now contain 3 items, check them
  theDB <- use db
  (storedBlocks, finRecs) <- liftIO $ do
    let
        env = theDB ^. storeEnv
        dbB = theDB ^. blockStore
        dbF = theDB ^. finalizationRecordStore
    sB <- transaction env True (flip databaseSize dbB)
    sB `shouldBe` 3
    sF <- transaction env True (flip databaseSize dbF)
    sF `shouldBe` 3
    storedBlocks <- transaction env True (flip loadAll dbB)
    finRecs <- map snd <$> transaction env True (flip loadAll dbF)
    return (storedBlocks, finRecs)
  blocks <- mapM (constructBlock . snd) storedBlocks
  liftIO $ do
    forM_ blocks (`shouldSatisfy` flip elem [blockPtr2, blockPtr, genesisBlock])
    forM_ finRecs (`shouldSatisfy` flip elem [frec2, frec, genesisFr])

  -- check the blocktable
  bs <- use (blockTable . at (bpHash blockPtr2))
  liftIO $ bs `shouldBe` Just (Concordium.GlobalState.Persistent.TreeState.BlockFinalized 2)

  -- check that the parent and last finalized are the proper ones
  parent <- bpParent blockPtr2
  liftIO $ parent `shouldBe` blockPtr
  lfin <- bpLastFinalized blockPtr2
  liftIO $ lfin `shouldBe` genesisBlock

testEmptyGS :: Test
testEmptyGS = do
  gb <- use genesisBlockPointer
  let gbh = bpHash gb

  theDB <- use db
  liftIO $ do
    let
        env = theDB ^. storeEnv
        dbB = theDB ^. blockStore
        dbF = theDB ^. finalizationRecordStore
    sB <- transaction env True (flip databaseSize dbB)
    sB `shouldBe` 1
    sF <- transaction env True (flip databaseSize dbF)
    sF `shouldBe` 1
  b <- readBlock gbh
  liftIO $ case b of
    Just b ->
      getHash (sbBlock b) `shouldBe` gbh
    _ -> expectationFailure "Genesis block is missing"
  fr <- readFinalizationRecord 0
  liftIO $ case fr of
    Just fr ->
      finalizationBlockPointer fr `shouldBe` gbh
    _ -> expectationFailure "Finalization record for genesis is missing"

tests :: Spec
tests = do
   describe "GlobalState:PersistentTreeState" $ do
    specifyWithGS "empty gs wrote the genesis to disk" testEmptyGS
    specifyWithGS "finalize a block" testFinalizeABlock
