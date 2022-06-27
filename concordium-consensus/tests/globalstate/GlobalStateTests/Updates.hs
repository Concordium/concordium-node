{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module GlobalStateTests.Updates where

{- What I want to test:
1. starting from an empty blockstate with the dummy parameters, try to register a baker with not enough stake. (MUST FAIL)
2. starting from an empty blockstate with the dummy parameters, register a baker with enough stake and
    2.1. decrease the stake below the limit. (MUST FAIL)
    2.2. decrease the stake above the limit. (MUST SUCCEED)
    2.3. increase the stake. (MUST SUCCEED)
3. starting from an empty blockstate with the dummy parameters, register a baker with enough stake, increase the limit and
    3.1. decrease the stake any amount (MUST FAIL)
    3.2. increase the stake below limit (MUST SUCCEED)
    3.3. increase the stake over limit (MUST SUCCEED)
-}

import Concordium.GlobalState
import Concordium.Types.AnonymityRevokers
import Concordium.GlobalState.Basic.BlockState as BS
import Concordium.GlobalState.Basic.BlockState.Account
import Concordium.GlobalState.Basic.TreeState
import Concordium.GlobalState.BlockPointer hiding (BlockPointer)
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.DummyData
import Concordium.Types.IdentityProviders
import Concordium.GlobalState.Paired
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.GlobalState.Persistent.BlockState as PBS
import Concordium.GlobalState.Persistent.TreeState
import Concordium.Logger
import Concordium.Types
import Control.Exception
import Control.Monad.Identity
import Control.Monad.RWS.Strict as RWS hiding (state)
import Control.Monad.Trans.Reader
import Data.IORef
import Data.Proxy
import Data.Time.Clock.POSIX
import Lens.Micro.Platform
import System.FilePath
import System.IO.Temp
import Test.Hspec

import qualified Concordium.Crypto.BlockSignature as BlockSig
import qualified Concordium.Crypto.VRF as VRF
import qualified Concordium.Crypto.BlsSignature as Bls
import Concordium.GlobalState.BakerInfo
import Concordium.Crypto.DummyData
import Concordium.Types.DummyData
import Concordium.ID.Parameters
import Concordium.ID.DummyData
import Concordium.Common.Time
import qualified Concordium.GlobalState.Persistent.BlockState.Updates as PU
import qualified Concordium.Types.UpdateQueues as UQ

--------------------------------------------------------------------------------
--                                                                            --
--                        Concrete monads for the test                        --
--                                                                            --
--------------------------------------------------------------------------------

type PV = 'P1

type PairedGSContext = PairGSContext () (PBS.PersistentBlockStateContext PV)

type PairedGState = PairGState
                      (SkovData PV (HashedBlockState PV))
                      (SkovPersistentData PV (PBS.HashedPersistentBlockState PV))

-- |Basically a PureBlockStateMonad and a PersistentBlockStateMonad paired
type ThisMonadConcrete = BlockStateM
                PV
                PairedGSContext
                (Identity PairedGSContext)
                PairedGState
                (Identity PairedGState)
                (RWST (Identity PairedGSContext) () (Identity PairedGState) LogIO)

type TheBlockStates = (BlockState PV, PBS.PersistentBlockState PV)

--------------------------------------------------------------------------------
--                                                                            --
--                           Create and destroy GS                            --
--                                                                            --
--------------------------------------------------------------------------------

createGS :: FilePath -> IO (Identity PairedGSContext, Identity PairedGState)
createGS dbDir = do
  now <- utcTimeToTimestamp <$> getCurrentTime
  let
    n = 3
    genesis = makeTestingGenesisDataP1 now
                                     n
                                     1
                                     1
                                     dummyFinalizationCommitteeMaxSize
                                     dummyCryptographicParameters
                                     emptyIdentityProviders
                                     emptyAnonymityRevokers
                                     maxBound
                                     dummyKeyCollection
                                     dummyChainParameters
    rp = defaultRuntimeParameters
    config = PairGSConfig (MTMBConfig rp, DTDBConfig rp dbDir (dbDir </> "blockstate" <.> "dat"))
  (x, y) <- runSilentLogger $ initialiseGlobalState genesis config
  return (Identity x, Identity y)

destroyGS :: (Identity PairedGSContext, Identity PairedGState) -> IO ()
destroyGS (Identity c, Identity s) =
  shutdownGlobalState (protocolVersion @PV)
                      (Proxy :: Proxy (PairGSConfig MemoryTreeMemoryBlockConfig DiskTreeDiskBlockConfig))
                      c
                      s

--------------------------------------------------------------------------------
--                                                                            --
--                                 Constants                                  --
--                                                                            --
--------------------------------------------------------------------------------

limit :: Amount
limit = dummyChainParameters ^. cpPoolParameters . ppBakerStakeThreshold
limitDelta :: AmountDelta
limitDelta = fromIntegral limit

--------------------------------------------------------------------------------
--                                                                            --
--                           Block state operations                           --
--                                                                            --
--------------------------------------------------------------------------------
-- Note this operations expect a @TheBlockStates@ as the last parameter which
-- makes them suitable to be chained with >>= later on.  As this is only for
-- testing, it uses always thomasAccount.

-- | Get the blockstates to perform operations on the current focusBlock (the dummy genesis)
getBlockStates :: ThisMonadConcrete TheBlockStates
getBlockStates = do
  bs <- get
  let -- get the blockstates
      bs1 = _unhashedBlockState $ _bpState (bs ^. pairStateLeft . Concordium.GlobalState.Basic.TreeState.focusBlock)
      bs2 = PBS.hpbsPointers $ _bpState (bs ^. pairStateRight . Concordium.GlobalState.Persistent.TreeState.focusBlock)
  return (bs1, bs2)

-- | Create the thomasAccount with a dummy credential and the provided amount. Return the account index of the newly created account
-- and the updated block state.
createAccountWith :: AmountDelta -> TheBlockStates -> ThisMonadConcrete (TheBlockStates, AccountIndex)
createAccountWith a bs = do
  (_, bs') <- bsoCreateAccount bs
                               dummyGlobalContext
                               thomasAccount
                               (dummyCredential dummyGlobalContext
                                                thomasAccount
                                                thomasVK
                                                (YearMonth 2021 01)
                                                (YearMonth 2021 12))
  ~(Just ai) <- bsoGetAccountIndex bs' thomasAccount
  (, ai) <$> bsoModifyAccount bs' (emptyAccountUpdate ai thomasAccount & auAmount ?~ a)

-- | Add a baker with the given staked amount.
addBakerWith :: Amount -> (TheBlockStates, AccountIndex) -> ThisMonadConcrete (BakerAddResult, (TheBlockStates, AccountIndex))
addBakerWith am (bs, ai) = do
  a <- BlockSig.verifyKey <$> liftIO BlockSig.newKeyPair
  b <- Bls.derivePublicKey <$> liftIO Bls.generateSecretKey
  c <- VRF.publicKey <$> liftIO VRF.newKeyPair
  (bar, bs') <- bsoAddBaker bs ai (BakerAdd (BakerKeyUpdate a b c) am False)
  return (bar, (bs', ai))
  

-- |Modify the staked amount to the given value.
modifyStakeTo :: Amount -> (TheBlockStates, AccountIndex) -> ThisMonadConcrete (BakerStakeUpdateResult, (TheBlockStates, AccountIndex))
modifyStakeTo a (bs, ai) = do
  (bsur, bs') <- bsoUpdateBakerStake bs ai a
  return (bsur, (bs', ai))

-- |Increase the current threshold for baking. This uses some trickery to run a
-- side monad that will be a MonadBlobStore that can retrieve the required
-- fields from the persistent block state and write them again after modifying
-- them. This is quite ad-hoc and probably should not be replicated in other
-- tests.
increaseLimit :: Amount -> (TheBlockStates, AccountIndex) -> ThisMonadConcrete (TheBlockStates, AccountIndex)
increaseLimit newLimit ((bs, bs2), ai) = do
  let f :: PBS.PersistentBlockStateMonad PV (PBS.PersistentBlockStateContext PV) (ReaderT (PBS.PersistentBlockStateContext PV) LogIO) ()
      f = do
        -- load the block from the IORef
        bsp <- refLoad =<< (liftIO $ readIORef bs2)
        -- load the updates field
        updates <- refLoad (PBS.bspUpdates bsp)
        -- load the current parameters
        currentParams <- unStoreSerialized <$> refLoad (PU.currentParameters updates)
        -- store the new parameters
        newParams <- refMake $ StoreSerialized (currentParams & cpPoolParameters . ppBakerStakeThreshold .~ newLimit)
        -- store the new updates
        newUpdates <- refMake (updates {PU.currentParameters = newParams })
        -- store the new block in the IORef
        liftIO . (writeIORef bs2) =<< refMake (bsp { PBS.bspUpdates = newUpdates })

  BlockStateM (-- This block is of type: RWST (Identity PairedGSContext) () (Identity PairedGState) LogIO) PBS.PersistentBlockState
               RWST (\(Identity (PairGSContext _ rc)) ps -> do
                        -- what we run here must generate a LogIO action that
                        -- can be wrapped in the RWST in order to have a
                        -- MonadBlobStore we need to have a
                        -- @PersistentBlockStateMonad r m@ where MonadIO m,
                        -- MonadReader r m, HasBlobStore r. ReaderT
                        -- PersistentBlockStateContext LogIO fulfills all the
                        -- requirements.
                        runReaderT (PBS.runPersistentBlockStateMonad f) rc
                        return ((), ps, ())))
  return ((bs & blockUpdates . UQ.currentParameters . cpPoolParameters . ppBakerStakeThreshold .~ newLimit, bs2), ai)

--------------------------------------------------------------------------------
--                                                                            --
--                                   Tests                                    --
--                                                                            --
--------------------------------------------------------------------------------

testing1, testing2'1, testing2'2, testing2'3, testing3'1, testing3'2, testing3'3 :: ThisMonadConcrete ()
-- starting from an empty blockstate with the dummy parameters, try to register
-- a baker with not enough stake. (MUST FAIL)
testing1 = do
  (res, _) <- getBlockStates >>=
              createAccountWith (limitDelta `div` 2) >>=
              addBakerWith (limit `div` 2)
  case res of
    BAStakeUnderThreshold -> return ()
    e -> error $ "Got (" ++ show e ++ ") but wanted BAStakeUnderThreshold"

-- starting from an empty blockstate with the dummy parameters, register a baker
-- with enough stake and decrease the stake below the limit. (MUST FAIL)
testing2'1 = do
  (res, _) <- getBlockStates >>=
              createAccountWith limitDelta >>=
              addBakerWith limit >>=
              \(BASuccess _, a) -> modifyStakeTo (limit - 1) a
  case res of
    BSUStakeUnderThreshold -> return ()
    e -> error $ "Got (" ++ show e ++ ") but wanted BSUStakeUnderThreshold"

-- starting from an empty blockstate with the dummy parameters, register a baker
-- with enough stake and decrease the stake above the limit. (MUST SUCCEED)
testing2'2 = do
  (res, _) <- getBlockStates >>=
              createAccountWith (limitDelta + 100) >>=
              addBakerWith (limit + 100) >>=
              \(BASuccess _, a) -> modifyStakeTo limit a
  case res of
    BSUStakeReduced _ _ -> return ()
    e -> error $ "Got (" ++ show e ++ ") but wanted BSUStakeReduced"

-- starting from an empty blockstate with the dummy parameters, register a baker
-- with enough stake and increase the stake. (MUST SUCCEED)
testing2'3 = do
  (res, _) <- getBlockStates >>=
              createAccountWith (limitDelta + 100) >>=
              addBakerWith limit >>=
              \(BASuccess _, a) -> modifyStakeTo (limit + 100) a
  case res of
    BSUStakeIncreased _ -> return ()
    e -> error $ "Got (" ++ show e ++ ") but wanted BSUStakeIncreased"

-- starting from an empty blockstate with the dummy parameters, register a baker
-- with enough stake, increase the limit and decrease the stake any amount (MUST
-- FAIL)
testing3'1 = do
  (res, _) <- getBlockStates >>=
              createAccountWith limitDelta >>=
              addBakerWith limit >>=
              (\(BASuccess _, a) -> increaseLimit (limit * 2) a) >>=
              modifyStakeTo (limit - 1)
  case res of
    BSUStakeUnderThreshold -> return ()
    e -> error $ "Got (" ++ show e ++ ") but wanted BSUStakeUnderThreshold"

-- starting from an empty blockstate with the dummy parameters, register a baker
-- with enough stake, increase the limit and increase the stake below limit
-- (MUST SUCCEED)
testing3'2 = do
  (res, _) <- getBlockStates >>=
              createAccountWith limitDelta >>=
              addBakerWith limit >>=
              (\(BASuccess _, a) -> increaseLimit (limit * 2) a) >>=
              modifyStakeTo (limit + 1 )
  case res of
    BSUStakeIncreased _ -> return ()
    e -> error $ "Got (" ++ show e ++ ") but wanted BakerStakeIncreased"

-- starting from an empty blockstate with the dummy parameters, register a baker
-- with enough stake, increase the limit and increase the stake over limit (MUST
-- SUCCEED)
testing3'3 = do
  (res, _) <- getBlockStates >>=
              createAccountWith limitDelta >>=
              addBakerWith limit >>=
              (\(BASuccess _, a) -> increaseLimit (limit * 2) a) >>=
              modifyStakeTo (limit * 2 + 1)
  case res of
    BSUStakeIncreased _ -> return ()
    e -> error $ "Got (" ++ show e ++ ") but wanted BakerStakeIncreased"

tests :: Spec
tests = do
  let wtdgs :: String -> ThisMonadConcrete () -> Spec
      wtdgs s t =  -- withTempGlobalState
        specify s $
          withTempDirectory "." "test-directory" $
            \dbDir -> bracket (createGS dbDir) destroyGS $
              \a -> (runSilentLogger . uncurry (runRWST (runBlockStateM t)) $ a) >> return ()

  describe "GlobalState.Updates - BakerStakeThreshold" $ do
    wtdgs "not enough stake - must fail" testing1
    wtdgs "enough stake >decrease> not enough - must fail" testing2'1
    wtdgs "enough stake >decrease> enough - must succeed" testing2'2
    wtdgs "enough stake >increase> enough - must succeed" testing2'3
    wtdgs "enough stake >> increase limit >decrease> not enough - must fail" testing3'1
    wtdgs "enough stake >> increase limit >increase> not enough - must succeed" testing3'2
    wtdgs "enough stake >> increase limit >increase> enough - must succeed" testing3'3
