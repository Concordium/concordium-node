{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module GlobalStateTests.PersistentTreeState where

import qualified Data.HashSet as HS
import qualified Data.List as List
import qualified Data.Sequence as Seq
import Data.Serialize

import Concordium.Crypto.BlsSignature
import Concordium.Crypto.DummyData
import qualified Concordium.Crypto.SHA256 as SHA256
import Concordium.Crypto.VRF as VRF
import Concordium.GlobalState
import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.BlockPointer hiding (BlockPointer)
import Concordium.GlobalState.Classes
import Concordium.GlobalState.DummyData
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.LMDB.Helpers
import Concordium.GlobalState.Parameters
import qualified Concordium.GlobalState.Persistent.BlockState as PBS
import Concordium.GlobalState.Persistent.LMDB
import Concordium.GlobalState.Persistent.TreeState
import Concordium.GlobalState.TreeState
import Concordium.Logger
import Concordium.Types
import Concordium.Types.AnonymityRevokers
import Concordium.Types.HashableTo
import Concordium.Types.IdentityProviders
import qualified Concordium.Types.Transactions as Trns
import Control.Exception
import Control.Monad.Identity
import Control.Monad.RWS.Strict as RWS hiding (state)
import qualified Data.FixedByteString as FBS
import Data.Time.Clock.POSIX
import Lens.Micro.Platform
import System.FilePath ((<.>), (</>))
import System.IO.Temp

import System.Random
import Test.Hspec
import Test.QuickCheck

-- |Protocol version.
type PV = 'P5

type TestM =
    PersistentTreeStateMonad
        (SkovPersistentData PV)
        ( PBS.PersistentBlockStateMonad
            PV
            (PBS.PersistentBlockStateContext PV)
            (RWST (PBS.PersistentBlockStateContext PV) () (SkovPersistentData PV) LogIO)
        )

type Test = TestM ()

instance HasGlobalStateContext (PBS.PersistentBlockStateContext PV) (PBS.PersistentBlockStateContext PV) where
    globalStateContext = id

instance HasGlobalState (SkovPersistentData PV) (SkovPersistentData PV) where
    globalState = id

createGlobalState :: FilePath -> IO (PBS.PersistentBlockStateContext PV, SkovPersistentData PV)
createGlobalState dbDir = do
    now <- utcTimeToTimestamp <$> getCurrentTime
    let
        n = 3
        genesis = makeTestingGenesisDataP5 now n 1 1 dummyFinalizationCommitteeMaxSize dummyCryptographicParameters emptyIdentityProviders emptyAnonymityRevokers maxBound dummyKeyCollection dummyChainParameters
        config = GlobalStateConfig defaultRuntimeParameters dbDir (dbDir </> "blockstate" <.> "dat")
    (x, y) <- runSilentLogger $ initialiseGlobalState genesis config
    return (x, y)

destroyGlobalState :: (PBS.PersistentBlockStateContext PV, SkovPersistentData PV) -> IO ()
destroyGlobalState (c, s) =
    shutdownGlobalState (protocolVersion @PV) c s

specifyWithGS :: String -> Test -> SpecWith (Arg Expectation)
specifyWithGS s f =
    specify s $
        withTempDirectory "." "test-directory" $
            \dbDir ->
                bracket (createGlobalState dbDir) destroyGlobalState $
                    runSilentLogger . void . uncurry (runRWST $ PBS.runPersistentBlockStateMonad $ runPersistentTreeStateMonad f)

useI :: MonadState (Identity s) f => Getting b s b -> f b
useI f = (^. f) . runIdentity <$> RWS.get

testFinalizeABlock :: Test
testFinalizeABlock = do
    (genesisBlock, genesisFr) :: (BlockPointerType TestM, FinalizationRecord) <- getLastFinalized
    sk <- liftIO $ generateSecretKey
    state <- blockState genesisBlock
    -- Create the block and finrec
    let proof1 = VRF.prove (fst $ randomKeyPair (mkStdGen 1)) "proof1"
    let proof2 = VRF.prove (fst $ randomKeyPair (mkStdGen 1)) "proof2"
    now <- liftIO $ getCurrentTime
    -- FIXME: Statehash is stubbed out with a placeholder hash
    pb <- makePendingBlock (fst $ randomBlockKeyPair (mkStdGen 1)) 1 (bpHash genesisBlock) 0 proof1 proof2 NoFinalizationData [] (StateHashV0 minBound) (getHash Trns.emptyTransactionOutcomesV0) now

    now' <- liftIO $ getCurrentTime
    blockPtr :: BlockPointerType TestM <- makeLiveBlock pb genesisBlock genesisBlock state now' 0
    let frec = FinalizationRecord 1 (bpHash blockPtr) (FinalizationProof [1] (sign "Hello" sk)) 0
    -- Add the finalization to the tree state
    mf <- markFinalized (bpHash blockPtr) frec
    addFinalization blockPtr frec
    wrapupFinalization frec [(mf, [])]
    -- Was updated as the last finalized?
    (b, fr) <- getLastFinalized
    liftIO $ do
        b `shouldBe` blockPtr
        fr `shouldBe` frec
    --- The database should now contain 2 items, check them
    theDB <- use db
    (storedBlocks, finRecs) <- liftIO $ do
        let env = theDB ^. storeEnv
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
    bs <- use (blockTable . liveMap . at (bpHash blockPtr))
    liftIO $ bs `shouldBe` Nothing -- block finalized, so should not be in the in-memory block table anymore
    -- check that the parent and last finalized are the proper ones
    parent <- bpParent blockPtr
    liftIO $ parent `shouldBe` genesisBlock
    lfin <- bpLastFinalized blockPtr
    liftIO $ lfin `shouldBe` genesisBlock
    -- add another block with different lfin and parent
    now'' <- liftIO $ getCurrentTime
    -- FIXME:  statehash is stubbed out with a palceholder stash
    pb2 <- makePendingBlock (fst $ randomBlockKeyPair (mkStdGen 1)) 2 (bpHash blockPtr) 0 proof1 proof2 NoFinalizationData [] (StateHashV0 minBound) (getHash Trns.emptyTransactionOutcomesV0) now''
    now''' <- liftIO $ getCurrentTime
    blockPtr2 :: BlockPointerType TestM <- makeLiveBlock pb2 blockPtr genesisBlock state now''' 0
    let frec2 = FinalizationRecord 2 (bpHash blockPtr2) (FinalizationProof [1] (sign "Hello" sk)) 0
    -- Add the finalization to the tree state
    mf2 <- markFinalized (bpHash blockPtr2) frec2
    addFinalization blockPtr2 frec2
    wrapupFinalization frec2 [(mf2, [])]
    --- The database should now contain 3 items, check them
    theDB <- use db
    (storedBlocks, finRecs) <- liftIO $ do
        let env = theDB ^. storeEnv
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
    bs <- use (blockTable . liveMap . at (bpHash blockPtr2))
    liftIO $ bs `shouldBe` Nothing -- block finalized, so should not be in the in-memory block table anymore
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
        let env = theDB ^. storeEnv
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

-- Tests for the dead block cache

-- |Generate random block hashes.
genBlockHashes :: Gen [BlockHash]
genBlockHashes = do
    len <- choose (700, 5000)
    replicateM len (BlockHash . SHA256.Hash . FBS.pack <$> vector 32)

-- |Generate distinct random block hashes. This uses repeated SHA256, which should reliably
-- avoid collisions.
genDistinctBlockHashes :: Gen [BlockHash]
genDistinctBlockHashes = do
    len <- choose (700 :: Int, 5000)
    init <- BlockHash . SHA256.Hash . FBS.pack <$> vector 32
    return $ doGen len init []
  where
    doGen len h0 l
        | len > 0 = let h = BlockHash (SHA256.hash (encode h0)) in doGen (len - 1) h (h : l)
        | otherwise = l

-- |Construct a cache by inserting the list of blocks from left to right.
constructCache :: [BlockHash] -> DeadCache
constructCache = List.foldl' (flip insertDeadCache) emptyDeadCache

-- |Test the following properties of the cache
-- - the queue and the hashmap are consistent (same length and elements)
-- - the size of the cache is bounded by 1000
testDeadCache :: Property
testDeadCache =
    withMaxSuccess 1000 $
        property $
            forAll genBlockHashes $ \bhs ->
                let dc = constructCache bhs
                in  HS.size (_dcHashes dc) === Seq.length (_dcQueue dc)
                        .&&. Seq.length (_dcQueue dc) <= 1000
                        .&&. all (`HS.member` _dcHashes dc) (_dcQueue dc)

-- |Test the following properties of the cache when the blocks inserted have distinct hashes.
-- - the last 1000 inserted blocks are always there
-- - the queue and the hashmap are consistent (same length and elements)
-- - the size of the cache is bounded by 1000
testDeadCacheDistinct :: Property
testDeadCacheDistinct =
    withMaxSuccess 1000 $
        property $
            forAll genDistinctBlockHashes $ \bhs ->
                let dc = constructCache bhs
                    lastThousand = drop (max 0 (length bhs - 1000)) bhs
                in  HS.size (_dcHashes dc) === Seq.length (_dcQueue dc)
                        .&&. Seq.length (_dcQueue dc) <= 1000
                        .&&. all (`HS.member` _dcHashes dc) (_dcQueue dc)
                        .&&. all (`memberDeadCache` dc) lastThousand

tests :: Spec
tests = do
    describe "GlobalState:PersistentTreeState" $ do
        specifyWithGS "empty gs wrote the genesis to disk" testEmptyGS
        specifyWithGS "finalize a block" testFinalizeABlock
        it "Dead block cache" testDeadCache
        it "Dead block cache (distinct hashes)" testDeadCacheDistinct
