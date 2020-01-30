{-# LANGUAGE DerivingVia, StandaloneDeriving, MultiParamTypeClasses, GeneralizedNewtypeDeriving, UndecidableInstances, FlexibleInstances, OverloadedStrings, ScopedTypeVariables #-}
module GlobalStateTests.PersistentTreeState where

import qualified Concordium.GlobalState.Persistent.BlockState as PBS
import Concordium.GlobalState.Persistent.TreeState
import Concordium.GlobalState.Classes
import Concordium.Crypto.BlsSignature
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.TreeState
import Concordium.GlobalState.BlockPointer
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Persistent.LMDB
import Control.Monad.State
import Concordium.GlobalState.BlockState
import Concordium.GlobalState
import GlobalStateTests.DummyData
import Control.Monad.RWS.Strict hiding (state)
import Control.Monad.IO.Class
import Data.Time.Clock.POSIX
import Control.Monad
import Control.Monad.Identity
import Data.Proxy
import Test.QuickCheck
import Test.Hspec
import Control.Exception
import Lens.Micro.Platform
import Data.Maybe
import Database.LMDB.Simple
import Database.LMDB.Simple.Extra
import System.Directory
import Concordium.Crypto.VRF as VRF
import Data.ByteString (ByteString)
import System.FilePath ((</>))

newtype MyTreeStateMonad c g s a = MyTreeStateMonad { runMTSM :: RWST c () s IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState s)

deriving via (GlobalStateM c (Identity c) g (Identity g) (RWST (Identity c) () (Identity s) IO))
  instance BlockStateTypes (MyTreeStateMonad c g s)

deriving via (GlobalStateM c (Identity c) g (Identity g) (RWST (Identity c) () (Identity s) IO))
  instance (BlockStateQuery (BlockStateM c (Identity c) g (Identity g) (RWST (Identity c) () (Identity s) IO))) =>
    BlockStateQuery (MyTreeStateMonad c g s)

deriving via (GlobalStateM c (Identity c) g (Identity g) (RWST (Identity c) () (Identity s) IO))
  instance (BlockStateOperations (BlockStateM c (Identity c) g (Identity g) (RWST (Identity c) () (Identity s) IO))) =>
    BlockStateOperations (MyTreeStateMonad c g s)

deriving via (GlobalStateM c (Identity c) g (Identity g) (RWST (Identity c) () (Identity s) IO))
  instance (BlockStateStorage (BlockStateM c (Identity c) g (Identity g) (RWST (Identity c) () (Identity s) IO))) =>
    BlockStateStorage (MyTreeStateMonad c g s)

deriving via (GlobalStateM c (Identity c) g (Identity g) (RWST (Identity c) () (Identity s) IO))
  instance (GlobalStateTypes (GlobalStateM c (Identity c) g (Identity g) (RWST (Identity c) () (Identity s) IO))) =>
    GlobalStateTypes (MyTreeStateMonad c g s)

deriving via (GlobalStateM c (Identity c) g (Identity g) (RWST (Identity c) () (Identity s) IO))
  instance (BlockPointerMonad (GlobalStateM c (Identity c) g (Identity g) (RWST (Identity c) () (Identity s) IO))) =>
    BlockPointerMonad (MyTreeStateMonad c g s)

deriving via (PersistentTreeStateMonad
              PBS.PersistentBlockState
              (MyTreeStateMonad
               PBS.PersistentBlockStateContext
               (SkovPersistentData PBS.PersistentBlockState)
               (SkovPersistentData PBS.PersistentBlockState)))
  instance (MonadState
            (SkovPersistentData PBS.PersistentBlockState)
            (MyTreeStateMonad
             PBS.PersistentBlockStateContext
             (SkovPersistentData PBS.PersistentBlockState)
             (SkovPersistentData PBS.PersistentBlockState))) =>
    LMDBStoreMonad (MyTreeStateMonad
                    PBS.PersistentBlockStateContext
                    (SkovPersistentData PBS.PersistentBlockState)
                    (SkovPersistentData PBS.PersistentBlockState))

deriving via (GlobalStateM c (Identity c) g (Identity g) (RWST (Identity c) () (Identity s) IO))
  instance (TreeStateMonad (GlobalStateM c (Identity c) g (Identity g) (RWST (Identity c) () (Identity s) IO)),
            BlockStateStorage (BlockStateM c (Identity c) g (Identity g) (RWST (Identity c) () (Identity s) IO))) =>
    TreeStateMonad (MyTreeStateMonad c g s)

type TestM = MyTreeStateMonad PBS.PersistentBlockStateContext (SkovPersistentData PBS.PersistentBlockState) (SkovPersistentData PBS.PersistentBlockState)
type Test = TestM ()

createGlobalState :: FilePath -> IO (PBS.PersistentBlockStateContext, SkovPersistentData PBS.PersistentBlockState)
createGlobalState dbDir = do
  now <- truncate <$> getPOSIXTime
  let
    n = 3
    genesis = makeGenesisData now n 1 0.5 1 dummyCryptographicParameters dummyIdentityProviders []
    state = genesisState genesis
    config = DTDBConfig defaultRuntimeParameters genesis state dbDir
  initialiseGlobalState config

destroyGlobalState :: (PBS.PersistentBlockStateContext, SkovPersistentData PBS.PersistentBlockState) -> IO ()
destroyGlobalState (c, s) = 
  shutdownGlobalState (Proxy :: Proxy DiskTreeDiskBlockConfig) c s

specifyWithGS :: String -> FilePath -> Test -> SpecWith (Arg (IO ()))
specifyWithGS s dbDir f = specify s $
                    bracket
                      (createGlobalState dbDir)
                      destroyGlobalState
                      (\(c, d) -> do
                          (ret, _, _) <- runRWST (runMTSM f) c d
                          return ret)

testFinalizeABlock :: Test
testFinalizeABlock = do
  (genesisBlock, genesisFr) :: (BlockPointer TestM, FinalizationRecord) <- getLastFinalized
  sk <- liftIO $ generateSecretKey
  state <- blockState genesisBlock
  -- Create the block and finrec
  proof1 <- liftIO $ VRF.prove (proofKP 1) "proof1"
  proof2 <- liftIO $ VRF.prove (proofKP 1) "proof2"
  now <- liftIO $ getCurrentTime
  pb <- makePendingBlock (kp 1) 1 (bpHash genesisBlock) 0 proof1 proof2 (bpHash genesisBlock) [] now
  now <- liftIO $ getCurrentTime
  blockPtr :: BlockPointer TestM <- makeLiveBlock pb genesisBlock genesisBlock state now 0
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
  env <- use (db . storeEnv)
  dbB <- use (db . blockStore)
  sB <- liftIO $ transaction env $ (size dbB :: Transaction ReadWrite Int)
  liftIO $ sB `shouldBe` 2
  dbF <- use (db . finalizationRecordStore)
  sF <- liftIO $ transaction env $ (size dbF :: Transaction ReadWrite Int)
  liftIO $ sF `shouldBe` 2
  blocksBytes <-  liftIO $ transaction env $ (elems dbB :: Transaction ReadWrite [ByteString])
  blocks <- mapM (constructBlock . Just) blocksBytes
  frecs <- liftIO $ transaction env $ (elems dbF :: Transaction ReadWrite [FinalizationRecord])
  mapM_ (\b -> liftIO $ should $ elem b [blockPtr, genesisBlock]) (catMaybes blocks)
  mapM_ (\b -> liftIO $ should $ elem b [frec, genesisFr]) frecs
  -- check the blocktable
  bs <- use (blockTable . at (bpHash blockPtr))
  liftIO $ bs `shouldBe` Just (Concordium.GlobalState.Persistent.TreeState.BlockFinalized 1)

  -- check that the parent and last finalized are the proper ones
  parent <- bpParent blockPtr
  liftIO $ parent `shouldBe` genesisBlock
  lfin <- bpLastFinalized blockPtr
  liftIO $ lfin `shouldBe` genesisBlock

  -- add another block with different lfin and parent
  now <- liftIO $ getCurrentTime
  pb2 <- makePendingBlock (kp 1) 2 (bpHash blockPtr) 0 proof1 proof2 (bpHash genesisBlock) [] now
  now <- liftIO $ getCurrentTime
  blockPtr2 :: BlockPointer TestM <- makeLiveBlock pb2 blockPtr genesisBlock state now 0
  let frec2 = FinalizationRecord 2 (bpHash blockPtr2) (FinalizationProof ([1], sign "Hello" sk)) 0

  -- Add the finalization to the tree state
  markFinalized (bpHash blockPtr2) frec2
  addFinalization blockPtr2 frec2

  --- The database should now contain 3 items, check them
  env <- use (db . storeEnv)
  dbB <- use (db . blockStore)
  sB <- liftIO $ transaction env $ (size dbB :: Transaction ReadWrite Int)
  liftIO $ sB `shouldBe` 3
  dbF <- use (db . finalizationRecordStore)
  sF <- liftIO $ transaction env $ (size dbF :: Transaction ReadWrite Int)
  liftIO $ sF `shouldBe` 3
  blocksBytes <-  liftIO $ transaction env $ (elems dbB :: Transaction ReadWrite [ByteString])
  blocks <- mapM (constructBlock . Just) blocksBytes
  frecs <- liftIO $ transaction env $ (elems dbF :: Transaction ReadWrite [FinalizationRecord])
  mapM_ (\b -> liftIO $ should $ elem b [blockPtr2, blockPtr, genesisBlock]) (catMaybes blocks)
  mapM_ (\b -> liftIO $ should $ elem b [frec2, frec, genesisFr]) frecs
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
  env <- use (db . storeEnv)
  dbB <- use (db . blockStore)
  sB <- liftIO $ transaction env $ (size dbB :: Transaction ReadWrite Int)
  liftIO $ sB `shouldBe` 1
  dbF <- use (db . finalizationRecordStore)
  sF <- liftIO $ transaction env $ (size dbF :: Transaction ReadWrite Int)
  liftIO $ sF `shouldBe` 1
  b <- readBlock gbh
  case b of
    Just b ->
      liftIO $ bpHash b `shouldBe` gbh
    _ -> liftIO $ failTest
  fr <- readFinalizationRecord 0
  case fr of
    Just fr ->
      liftIO $ finalizationBlockPointer fr `shouldBe` gbh
    _ -> liftIO $ failTest

tests :: Spec
tests = do
  dbPath <- runIO $ (</> "databases") `fmap` getCurrentDirectory 
  around (
    bracket
      (createDirectoryIfMissing False dbPath)
      (\_ -> removePathForcibly dbPath)) $
   describe "GlobalState:PersistentTreeState" $ do
    specifyWithGS "empty gs wrote the genesis to disk" dbPath testEmptyGS
    specifyWithGS "finalize a block" dbPath testFinalizeABlock
