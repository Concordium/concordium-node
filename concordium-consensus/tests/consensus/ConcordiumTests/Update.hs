{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-deprecations #-}

-- |This file contains test cases for failure states in Concordium.Skov.Update.
-- Specifically, fail state testing for the majority of the reasons 'doStoreBlock' and 'addBlock' will reject blocks.
-- Structure is that one baker will mine two blocks, then a second treestate will accept the first block, 
-- falsify some element of the second block, then check to make sure the dirtied second block is rejected.
--
-- Does not currently verify which error gets thrown, could refactor to do this?
module ConcordiumTests.Update where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Time.Clock.POSIX
import Data.Time.Clock
import Lens.Micro.Platform

import Test.Hspec

import Concordium.Afgjort.Finalize

import Concordium.Birk.Bake

import qualified Concordium.Crypto.BlockSignature as Sig
import Concordium.Crypto.SHA256

import Concordium.GlobalState
import Concordium.GlobalState.Basic.BlockState.Account
import Concordium.GlobalState.BakerInfo
import Concordium.Types.IdentityProviders
import Concordium.GlobalState.Block
import qualified Concordium.GlobalState.BlockPointer as BS
import Concordium.GlobalState.Parameters
import qualified Concordium.Types.SeedState as SeedState

import Concordium.Logger

import Concordium.Skov.Monad
import Concordium.Skov.MonadImplementations

import Concordium.Startup (defaultFinalizationParameters, makeBakersByStake)

import Concordium.Types

import Data.FixedByteString as FBS
import Concordium.Crypto.SHA256 as Hash

import qualified Concordium.GlobalState.DummyData as Dummy
        
dummyTime :: UTCTime
dummyTime = posixSecondsToUTCTime 0

type Config t = SkovConfig MemoryTreeMemoryBlockConfig (ActiveFinalization t) NoHandler

finalizationParameters :: FinalizationParameters
finalizationParameters = defaultFinalizationParameters{finalizationMinimumSkip=100} -- setting minimum skip to 100 to prevent finalizers to finalize blocks when they store them

type MyHandlers = SkovHandlers DummyTimer (Config DummyTimer) (StateT () LogIO)

newtype DummyTimer = DummyTimer Integer

type MySkovT = SkovT MyHandlers (Config DummyTimer) (StateT () LogIO)

instance MonadFail MySkovT where
  fail = error

dummyHandlers :: MyHandlers
dummyHandlers = SkovHandlers {..}
    where
        shBroadcastFinalizationMessage _ = return ()
        shBroadcastFinalizationRecord _ = return ()
        shOnTimeout _ _ = return $ DummyTimer 0
        shCancelTimer _ = return ()
        shPendingLive = return ()

myRunSkovT :: (MonadIO m)
           => MySkovT a
           -> MyHandlers
           -> SkovContext (Config DummyTimer)
           -> SkovState (Config DummyTimer)
           -> m (a, SkovState (Config DummyTimer), ())
myRunSkovT a handlers ctx st = liftIO $ flip runLoggerT doLog $ do
        ((res, st'), _) <- runStateT (runSkovT a handlers ctx st) ()
        return (res, st', ())
    where
        doLog src LLError msg = error $ show src ++ ": " ++ msg
        doLog _ _ _ = return () -- traceM $ show src ++ ": " ++ msg

type BakerState = (BakerIdentity, SkovContext (Config DummyTimer), SkovState (Config DummyTimer))
type BakerInformation = (FullBakerInfo, BakerIdentity, Account)

-- |Create initial states for two bakers
createInitStates :: IO (BakerState, BakerState)
createInitStates = do
    let bakerAmount = 10 ^ (4 :: Int)
        bis@[baker1, baker2] = makeBakersByStake [bakerAmount, bakerAmount]
        seedState = SeedState.initialSeedState (hash "LeadershipElectionNonce") 10
        bakerAccounts = (^. _3) <$> bis
        cps = Dummy.dummyChainParameters & cpElectionDifficulty .~ makeElectionDifficultyUnchecked 100000
        gen = GenesisDataV2 {
                genesisTime = 0,
                genesisSlotDuration = 1,
                genesisSeedState = seedState,
                genesisAccounts = bakerAccounts,
                genesisFinalizationParameters = finalizationParameters,
                genesisCryptographicParameters = Dummy.dummyCryptographicParameters,
                genesisIdentityProviders = emptyIdentityProviders,
                genesisAnonymityRevokers = Dummy.dummyArs,
                genesisMaxBlockEnergy = Energy maxBound,
                genesisAuthorizations = Dummy.dummyAuthorizations,
                genesisChainParameters = cps
            }
        createState = liftIO . (\(bid, _, _, _) -> do
                                   let fininst = FinalizationInstance (bakerSignKey bid) (bakerElectionKey bid) (bakerAggregationKey bid)
                                       config = SkovConfig
                                           (MTMBConfig defaultRuntimeParameters gen)
                                           (ActiveFinalization fininst)
                                           NoHandler
                                   (initCtx, initState) <- runSilentLogger (initialiseSkov config)
                                   return (bid, initCtx, initState))
    b1 <- createState baker1
    b2 <- createState baker2
    return (b1, b2)

instance Show BakerIdentity where
    show _ = "[Baker Identity]"

withInitialStates :: (BakerState -> BakerState -> IO ()) -> IO ()
withInitialStates r = do
    (b1, b2) <- createInitStates
    r b1 b2

-- |Helper function to resign blocks after dirtying
reSign :: BakerSignPrivateKey -> BakedBlock -> BakedBlock
reSign key BakedBlock{..}  = BakedBlock{bbSignature = newSig, ..}
    where
        BlockFields{..} = bbFields
        newBlockHash = generateBlockHash bbSlot bfBlockPointer bfBlockBaker bfBlockBakerKey bfBlockProof bfBlockNonce bfBlockFinalizationData bbTransactions bbStateHash bbTransactionOutcomesHash
        newSig = Sig.sign key (Hash.hashToByteString (v0BlockHash newBlockHash))


-- |Helper function to bake
bake :: BakerIdentity -> Slot -> MySkovT BakedBlock
bake bid n = do
    mb <- bakeForSlot bid n
    maybe (fail $ "Could not bake for slot " ++ show n)
          (\BS.BlockPointer {_bpBlock = NormalBlock block} -> return block)
          mb

-- |Attempts to store a block, and throws an error if it fails
store :: (SkovMonad m, MonadFail m) => BakedBlock -> m ()
store block = storeBlock (makePendingBlock block dummyTime) >>= \case
    ResultSuccess -> return()
    result        -> fail $ "Failed to store un-dirtied block " ++ show block ++ ". Reason: " ++ show result

-- |Attempts to store a block, and throws an error if it succeeds
-- Used for verifying that dirtied blocks are rejected
failStore :: (SkovMonad m, MonadFail m) => BakedBlock -> m ()
failStore block = storeBlock (makePendingBlock block dummyTime) >>= \case
    ResultSuccess -> fail $ "Successfully stored dirtied block: " ++ show block
    _        -> return()

-- * Helper functions for dirtying fields of blocks

stubBlockHash :: BlockHash
stubBlockHash = BlockHashV0 (Hash (FBS.pack (Prelude.replicate 32 (fromIntegral (3 :: Word)))))

stubStateHash :: StateHash
stubStateHash = StateHashV0 (Hash (FBS.pack (Prelude.replicate 32 (fromIntegral (3 :: Word)))))

stubTransactionHash :: TransactionOutcomesHash
stubTransactionHash = TransactionOutcomesHashV0 (Hash (FBS.pack (Prelude.replicate 32 (fromIntegral (3 :: Word)))))

-- * Dirtying functions to dirty blocks, allowing testing of each individual reject condition in update

dirtyTransactionOutcomesHash :: BakedBlock -> BakerSignPrivateKey -> BakedBlock
dirtyTransactionOutcomesHash BakedBlock{..} bid = reSign bid BakedBlock{bbTransactionOutcomesHash = stubTransactionHash, ..}

dirtyStateHash ::  BakedBlock -> BakerSignPrivateKey -> BakedBlock
dirtyStateHash BakedBlock{..} bid = reSign bid BakedBlock{bbStateHash = stubStateHash, ..}

-- |Dirties the claimed key so it doesn't match the signature anymore
dirtyBakerKey :: BakedBlock -> BakerSignPrivateKey -> BakedBlock
dirtyBakerKey BakedBlock{..} _ = BakedBlock{bbFields = BlockFields{bfBlockBakerKey = fakeVerifKey,..}, ..}
    where
        BlockFields{..} = bbFields
        baker3 = Dummy.mkFullBaker 2 0
        fakeVerifKey = baker3 ^. _1 . bakerInfo . bakerSignatureVerifyKey

-- |Dirties the key, and resigns the block with the dirtied key. This tests that we verify the key is the same as baker key
dirtyBakerKeySignature :: BakedBlock -> BakerSignPrivateKey -> BakedBlock
dirtyBakerKeySignature BakedBlock{..} _ = reSign fakeKeyPair BakedBlock{bbFields = BlockFields{bfBlockBakerKey = fakeVerifyKey,..}, ..}
    where
        BlockFields{..} = bbFields
        baker3 = Dummy.mkFullBaker 2 0
        fakeVerifyKey = baker3 ^. _1 . bakerInfo . bakerSignatureVerifyKey
        fakeKeyPair = Sig.KeyPair{ signKey = baker3 ^._3, verifyKey = fakeVerifyKey }



-- |Claims earlier slot than reality
dirtySlot1 ::BakedBlock -> BakerSignPrivateKey -> BakedBlock
dirtySlot1 BakedBlock{..} bid = reSign bid BakedBlock{bbSlot = 1, ..} 

-- |Claims later slot than reality
dirtySlot2 :: BakedBlock -> BakerSignPrivateKey -> BakedBlock
dirtySlot2 BakedBlock{..} bid = reSign bid BakedBlock{bbSlot = 3, ..} 


-- |Sanity check test, to make sure that an undirtied block doesn't fail to be stored
runSanityCheck :: BakerState
        -- ^Initial state for the first baker
        -> BakerState
        -> IO ()
runSanityCheck (bid1, fi1, fs1) (_, fi2, fs2) = do
            -- baker1 bakes some blocks to create tree
            (block1, fs1', _) <- myRunSkovT (bake bid1 1) dummyHandlers fi1 fs1
            (block2, _, _) <- myRunSkovT (bake bid1 2) dummyHandlers fi1 fs1'
            void $ myRunSkovT (do
                    -- Baker 2 adds both blocks to his tree
                    store block1
                    store block2
                    ) dummyHandlers fi2 fs2

-- |Sets up by baker1 baking two blocks. baker2 then adds the first block to his tree
-- The second block is dirtied in a way defined by dirtyFunc, which modifies a block then resigns it using baker1's key.
--  then we check to make sure that storage of the second block fails using failStore
runTest :: (BakedBlock -> BakerSignPrivateKey -> BakedBlock)
        -> BakerState
        -- ^Initial state for the first baker
        -> BakerState
        -> IO ()
runTest dirtyFunc (bid1, fi1, fs1) (_, fi2, fs2) = do
            -- baker1 bakes some blocks to create tree
            (block1, fs1', _) <- myRunSkovT (bake bid1 1) dummyHandlers fi1 fs1
            (block2, _, _) <- myRunSkovT (bake bid1 2) dummyHandlers fi1 fs1'
            void $ myRunSkovT (do
                    -- Baker2 stores the first block
                    store block1
                    -- Second block is dirtied before attempting to store, then we make sure it gets rejected
                    failStore (dirtyFunc block2 (bakerSignKey bid1))
                    ) dummyHandlers fi2 fs2


-- |Tests sending duplicate blocks
runTestDupe :: BakerState
        -- ^Initial state for the first baker
        -> BakerState
        -> IO ()
runTestDupe (bid1, fi1, fs1) (_, fi2, fs2) = do
            -- baker1 bakes some blocks to create tree
            (block1, fs1', _) <- myRunSkovT (bake bid1 1) dummyHandlers fi1 fs1
            (block2, _, _) <- myRunSkovT (bake bid1 2) dummyHandlers fi1 fs1'
            void $ myRunSkovT (do
                    -- Baker 2 adds both blocks to his tree
                    store block1
                    store block2
                    failStore block2
                    ) dummyHandlers fi2 fs2


test :: Spec
test = describe "Concordium.Update: " $ do
    it "Un-dirtied Baked Block should not be rejected" $ withInitialStates runSanityCheck
    it "Block with incorrect TransactionOutcomesHash should be rejected" $ withInitialStates (runTest dirtyTransactionOutcomesHash)
    it "Block with incorrect StateHash should be rejected" $ withInitialStates (runTest dirtyStateHash)
    it "Block with incorrect earlier Slot Number should be rejected" $ withInitialStates (runTest dirtySlot1)
    it "Block with incorrect later Slot Number should be rejected" $ withInitialStates (runTest dirtySlot2)
    it "Block with Claimed key which doesn't match signature should be rejected" $ withInitialStates (runTest dirtyBakerKey)
    it "Block with Claimed key which matches signature/blockhash, but is different from BakerKey should be rejected" $ withInitialStates (runTest dirtyBakerKeySignature)
    it "Duplicate Block should be rejected" $ withInitialStates runTestDupe

