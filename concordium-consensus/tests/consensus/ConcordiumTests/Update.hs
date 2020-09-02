{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-deprecations #-}
module ConcordiumTests.Update where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (fromMaybe)
import Data.Time.Clock.POSIX
import Data.Time.Clock
import Lens.Micro.Platform
import System.IO.Unsafe
import System.Random

import Test.QuickCheck
import Test.Hspec

import Concordium.Afgjort.Finalize

import Concordium.Birk.Bake

import qualified Concordium.Crypto.BlockSignature as Sig
import qualified Concordium.Crypto.BlsSignature as Bls
import Concordium.Crypto.DummyData
import Concordium.Crypto.SHA256
import qualified Concordium.Crypto.VRF as VRF

import Concordium.GlobalState
import Concordium.GlobalState.Basic.BlockState.Account
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.Basic.BlockState.Bakers
import Concordium.GlobalState.IdentityProviders
import Concordium.GlobalState.AnonymityRevokers
import Concordium.GlobalState.Block
import qualified Concordium.GlobalState.BlockPointer as BS
import Concordium.GlobalState.Parameters
import qualified Concordium.GlobalState.SeedState as SeedState

import Concordium.Logger

import Concordium.Skov.Monad
import Concordium.Skov.MonadImplementations

import Concordium.Startup (makeBakerAccount, defaultFinalizationParameters)

import Concordium.Types

import Data.FixedByteString as FBS

import qualified Concordium.GlobalState.DummyData as Dummy
import qualified Concordium.Types.DummyData as DummyTypes

-- Setup dummy values for stubbing environment 
{-# NOINLINE dummyCryptographicParameters #-}
dummyCryptographicParameters :: CryptographicParameters
dummyCryptographicParameters =
    fromMaybe
        (error "Could not read cryptographic parameters.")
        (unsafePerformIO (getExactVersionedCryptographicParameters <$> BSL.readFile "../scheduler/testdata/global.json"))
        
dummyArs :: AnonymityRevokers
dummyArs = emptyAnonymityRevokers

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

makeBaker :: Amount -> BakerId -> Gen BakerInformation
makeBaker initAmount bid = resize 0x20000000 $ do
        ek@(VRF.KeyPair _ epk) <- arbitrary
        sk                     <- genBlockKeyPair
        blssk                  <- fst . randomBlsSecretKey . mkStdGen <$> arbitrary
        let spk     = Sig.verifyKey sk
        let blspk   = Bls.derivePublicKey blssk
        let account = makeBakerAccount bid initAmount
        return (FullBakerInfo (BakerInfo epk spk blspk (account ^. accountAddress)) initAmount, BakerIdentity sk ek blssk, account)

-- Create initial states for two bakers
createInitStates :: IO (BakerState, BakerState)
createInitStates = do
    let bakerAmount = 10 ^ (4 :: Int)
    baker1 <- generate $ makeBaker bakerAmount 0
    baker2 <- generate $ makeBaker bakerAmount 1
    let bis = baker1 : [baker2] 
        genesisBakers = fst . bakersFromList $ (^. _1) <$> bis
        seedState = SeedState.genesisSeedState (hash "LeadershipElectionNonce") 10
        elDiff = 1
        bakerAccounts = map (\(_, _, acc) -> acc) bis
        gen = GenesisData 0 1 genesisBakers seedState elDiff bakerAccounts [] finalizationParameters dummyCryptographicParameters emptyIdentityProviders dummyArs 10 $ Energy maxBound
        createState = liftIO . (\(_, bid, _) -> do
                                   let fininst = FinalizationInstance (bakerSignKey bid) (bakerElectionKey bid) (bakerAggregationKey bid)
                                       config = SkovConfig
                                           (MTMBConfig defaultRuntimeParameters gen (Dummy.basicGenesisState gen))
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

-- Helper function to bake
bake :: BakerIdentity -> Slot -> MySkovT BakedBlock
bake bid n = do
    mb <- bakeForSlot bid n
    maybe (fail $ "Could not bake for slot " ++ show n)
          (\BS.BlockPointer {_bpBlock = NormalBlock block} -> return block)
          mb

-- Attempts to store a block, and throws an error if it fails
store :: (SkovMonad m, MonadFail m) => BakedBlock -> m ()
store block = storeBlock (makePendingBlock block dummyTime) >>= \case
    ResultSuccess -> return()
    result        -> fail $ "Failed to store un-dirtied block " ++ show block ++ ". Reason: " ++ show result

-- Attempts to store a block, and throws an error if it succeeds
-- Used for verifying that dirtied blocks are rejected
failStore :: (SkovMonad m, MonadFail m) => BakedBlock -> m ()
failStore block = storeBlock (makePendingBlock block dummyTime) >>= \case
    ResultSuccess -> fail $ "Successfully stored dirtied block: " ++ show block
    _        -> return()

-- Helper functions for dirtying fields of blocks
stubBlockHash :: BlockHash
stubBlockHash = BlockHashV0 (Hash (FBS.pack (Prelude.replicate 32 (fromIntegral (3 :: Word)))))

stubStateHash :: StateHash
stubStateHash = StateHashV0 (Hash (FBS.pack (Prelude.replicate 32 (fromIntegral (3 :: Word)))))

stubTransactionHash :: TransactionOutcomesHash
stubTransactionHash = TransactionOutcomesHashV0 (Hash (FBS.pack (Prelude.replicate 32 (fromIntegral (3 :: Word)))))

-- Dirtying functions to dirty blocks, allowing testing of each individual reject condition in update
dirtyTransactionHash :: BakedBlock -> BakedBlock
dirtyTransactionHash BakedBlock{..} = BakedBlock{bbTransactionOutcomesHash = stubTransactionHash, ..}

dirtyStateHash ::  BakedBlock -> BakedBlock
dirtyStateHash BakedBlock{..} = BakedBlock{bbStateHash = stubStateHash, ..}


dirtyBakerKey :: BakedBlock -> BakedBlock
dirtyBakerKey BakedBlock{..}= BakedBlock{bbFields = BlockFields{bfBlockBakerKey = fakeKey,..}, ..}
    where
        BlockFields{..} = bbFields
        baker3 = Dummy.mkFullBaker 2 DummyTypes.thomasAccount
        fakeKey = baker3 ^. _1 . bakerInfo . bakerSignatureVerifyKey


-- Claims earlier slot than reality
dirtySlot1 :: BakedBlock -> BakedBlock
dirtySlot1 BakedBlock{..} = BakedBlock{bbSlot = 1, ..} 

-- Claims later slot than reality
dirtySlot2 :: BakedBlock -> BakedBlock
dirtySlot2 BakedBlock{..} = BakedBlock{bbSlot = 3, ..} 


-- Sanity check test, to make sure that an undirtied block doesn't fail to be stored
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

-- Sets up by baker1 baking two blocks. baker2 then adds the first block to his tree
-- The second block is dirtied in a way defined by dirtyFunc, then we check 
-- to make sure that storage of the second block fails using failStore
runTest :: (BakedBlock -> BakedBlock)
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
                    failStore (dirtyFunc block2)
                    ) dummyHandlers fi2 fs2

-- Tests sending duplicate blocks
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
    it "Block with incorrect TransactionHash should be rejected" $ withInitialStates (runTest dirtyTransactionHash) 
    it "Block with incorrect StateHash should be rejected" $ withInitialStates (runTest dirtyStateHash) 
    it "Block with incorrect earlier Slot Number should be rejected" $ withInitialStates (runTest dirtySlot1) 
    it "Block with incorrect later Slot Number should be rejected" $ withInitialStates (runTest dirtySlot2) 
    it "Block with Claimed key different from BakerKey should be rejected" $ withInitialStates (runTest dirtyBakerKey) 
    it "Duplicate Block should be rejected" $ withInitialStates (runTestDupe)

