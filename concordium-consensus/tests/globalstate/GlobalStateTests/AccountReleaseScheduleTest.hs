{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module GlobalStateTests.AccountReleaseScheduleTest where

import Concordium.GlobalState
import Concordium.GlobalState.AccountTransactionIndex
import Concordium.Types.AnonymityRevokers
import Concordium.GlobalState.Basic.BlockState as BS
import Concordium.GlobalState.Basic.BlockState.Account
import Concordium.GlobalState.Basic.BlockState.AccountReleaseSchedule
import qualified Concordium.GlobalState.Basic.BlockState.AccountTable as AT
import Concordium.GlobalState.Basic.BlockState.Accounts
import Concordium.GlobalState.Basic.TreeState
import Concordium.GlobalState.BlockPointer hiding (BlockPointer)
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.DummyData
import Concordium.Types.IdentityProviders
import Concordium.GlobalState.Paired
import Concordium.GlobalState.Parameters
import qualified Concordium.GlobalState.Persistent.Accounts
import qualified Concordium.GlobalState.Persistent.Account
import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.GlobalState.Persistent.BlockState as PBS
import Concordium.GlobalState.Persistent.TreeState
import Concordium.Logger
import Concordium.Types
import Concordium.Types.HashableTo
import Control.Exception
import Control.Monad
import Control.Monad.Identity
import Control.Monad.RWS.Strict as RWS hiding (state)
import qualified  Control.Monad.Reader.Class as R
import Control.Monad.Trans.Reader
import Data.Aeson (eitherDecode)
import Data.Foldable
import Data.IORef
import Data.List (nub, sort)
import qualified Data.Map.Strict as OrdMap
import Data.Proxy
import Data.Time.Clock.POSIX
import qualified Data.Vector as Vector
import Lens.Micro.Platform
import System.FilePath
import System.IO.Temp
import Test.Hspec
import Test.QuickCheck

--------------------------------- Monad Types ----------------------------------

-- |Protocol version.
type PV = 'P1

type PairedGSContext = PairGSContext () PBS.PersistentBlockStateContext

type PairedGState = PairGState
                      (SkovData PV (HashedBlockState PV))
                      (SkovPersistentData PV () (PBS.HashedPersistentBlockState PV))

type ThisMonadConcrete = BlockStateM
                PV
                PairedGSContext
                (Identity PairedGSContext)
                PairedGState
                (Identity PairedGState)
                (RWST (Identity PairedGSContext) () (Identity PairedGState) LogIO)

--------------------------------- Test values ----------------------------------

genTimestamp :: Gen Timestamp
genTimestamp = Timestamp <$> arbitrary

dummyTransactionHash :: TransactionHash
dummyTransactionHash = case eitherDecode "\"f26a45adbb7d5cbefd9430d1eac665bd225fb3d8e04efb288d99a0347f0b8868\"" of
  Left e -> error e
  Right v -> v

transactions :: Int -> IO [[(Timestamp, Amount)]]
transactions numOfSchedules = do
  let numReleasesInEach = map getPositive <$> vectorOf numOfSchedules arbitrary
      listsOfTimestamps = fmap (sort . nub . map (`mod` 200)) . flip vectorOf genTimestamp
      listsOfTuples = mapM (flip fmap ((`mod` 200) . getPositive <$> arbitrary) . (,))
  generate $ numReleasesInEach >>= mapM (listsOfTimestamps >=> listsOfTuples)

setOfTransactions :: Int -> IO ([[(Timestamp,Amount)]], OrdMap.Map Timestamp Amount, [Timestamp])
setOfTransactions numTxs = do
  txs <- transactions numTxs
  let amounts = OrdMap.fromListWith (+) . concat $ txs
  let timestamps = OrdMap.keys amounts
  return (txs, amounts, timestamps)

------------------------------- Helper functions -------------------------------

createGS :: FilePath -> IO (Identity PairedGSContext, Identity PairedGState)
createGS dbDir = do
  now <- utcTimeToTimestamp <$> getCurrentTime
  let
    n = 3
    genesis = makeTestingGenesisDataP1 now n 1 1 dummyFinalizationCommitteeMaxSize dummyCryptographicParameters emptyIdentityProviders emptyAnonymityRevokers maxBound dummyKeyCollection dummyChainParameters
    rp = defaultRuntimeParameters { rpTreeStateDir = dbDir, rpBlockStateFile = dbDir </> "blockstate" }
    config = PairGSConfig (MTMBConfig rp genesis, DTDBConfig rp genesis)
  (x, y, (NoLogContext, NoLogContext)) <- runSilentLogger $ initialiseGlobalState config
  return (Identity x, Identity y)

destroyGS :: (Identity PairedGSContext, Identity PairedGState) -> IO ()
destroyGS (Identity c, Identity s) = shutdownGlobalState (Proxy :: Proxy (PairGSConfig (MemoryTreeMemoryBlockConfig PV) (DiskTreeDiskBlockConfig PV))) c s (NoLogContext, NoLogContext)

------------------------------------- Test -------------------------------------

testing :: ThisMonadConcrete ()
testing = do
  (!txsA, !amountsA, !timestampsA) <- liftIO $ setOfTransactions 3
  (!txsB, !amountsB, !timestampsB) <- liftIO $ setOfTransactions 2
  bs <- get
  let -- get the blockstates
      bs1 = _unhashedBlockState $ _bpState (bs ^. pairStateLeft . Concordium.GlobalState.Basic.TreeState.focusBlock)
      bs2 = PBS.hpbsPointers $ _bpState (bs ^. pairStateRight . Concordium.GlobalState.Persistent.TreeState.focusBlock)

  let [(accA, _), (accB, _)] = take 2 $ map (\(_, ac) -> (ac ^. accountAddress, ac ^. accountAmount)) $ AT.toList $ accountTable (bs1 ^. blockAccounts)
  b' <- bsoAddReleaseSchedule (bs1, bs2) [(accA, head timestampsA), (accB, head timestampsB)]
  checkEqualBlockReleaseSchedule b'
  bs'' <- foldlM (\b e@(acc, rels) -> do
                     newB <- bsoModifyAccount b ((emptyAccountUpdate acc) { _auReleaseSchedule = Just [(rels, dummyTransactionHash)] })
                     checkCorrectAccountReleaseSchedule newB b e
                     return newB
                 ) b' (map (accA,) txsA ++ map (accB,) txsB)
  checkEqualBlockReleaseSchedule bs''
  let taggedAmounts = OrdMap.toAscList $ OrdMap.unionWith (++) (OrdMap.map ((:[]) . (accA,)) amountsA) (OrdMap.map ((:[]) . (accB,)) amountsB)
  _ <- foldlM (\b (ts, accs) -> do
            newB <- bsoProcessReleaseSchedule b ts
            checkCorrectShrinkingAccountReleaseSchedule newB b ts accs
            return newB) bs'' taggedAmounts
  return ()


tests :: Spec
tests = do
  describe "GlobalState.AccountReleaseScheduleTest"
    $ specify "correct releases"
    $ withTempDirectory "." "test-directory"
    $ \dbDir -> bracket (createGS dbDir) destroyGS $ \a -> do
    _ <- runSilentLogger . uncurry (runRWST (runBlockStateM testing)) $ a
    return ()

------------------------------------ Checks ------------------------------------

-- | Check that the two implementations of blockstate have the same Block release schedule
checkEqualBlockReleaseSchedule :: (BS.BlockState PV, PBS.PersistentBlockState PV) -> ThisMonadConcrete ()
checkEqualBlockReleaseSchedule (blockstateBasic, blockStatePersistent) = do
  let brs = _blockReleaseSchedule blockstateBasic
  ctx <- PBS.pbscBlobStore . _pairContextRight <$> R.ask
  brsP <- liftIO $ runReaderT (do
                                 blockStatePersistent' <- loadBufferedRef =<< liftIO (readIORef  blockStatePersistent)
                                 loadBufferedRef $ PBS.bspReleaseSchedule  blockStatePersistent') ctx
  assert (brs == brsP) $ return ()

-- | Check that an account has the same Account release schedule in the two implementations of the blockstate
checkEqualAccountReleaseSchedule :: (BS.BlockState PV, PBS.PersistentBlockState PV) -> AccountAddress -> ThisMonadConcrete ()
checkEqualAccountReleaseSchedule (blockStateBasic, blockStatePersistent) acc = do
  let Just newBasicAccount = Concordium.GlobalState.Basic.BlockState.Accounts.getAccount acc (blockStateBasic ^. blockAccounts)
  ctx <- PBS.pbscBlobStore . _pairContextRight <$> R.ask
  newPersistentAccountReleaseScheduleHash <- liftIO $ runReaderT (do
                                                  blockStatePersistent' <- loadBufferedRef =<< liftIO (readIORef  blockStatePersistent)
                                                  Concordium.GlobalState.Persistent.Accounts.getAccount acc (PBS.bspAccounts blockStatePersistent') >>= \case
                                                    Nothing -> return Nothing
                                                    Just a -> Just <$> (loadBufferedRef (Concordium.GlobalState.Persistent.Account._accountReleaseSchedule a) >>= getHashM)) ctx :: ThisMonadConcrete (Maybe AccountReleaseScheduleHash)
  assert (Just (getHash (newBasicAccount ^. accountReleaseSchedule)) == newPersistentAccountReleaseScheduleHash) $ return ()

-- | Check that an an account was correctly updated in the two blockstates with the given release schedule
checkCorrectAccountReleaseSchedule :: (BS.BlockState PV, PBS.PersistentBlockState PV) -> (BS.BlockState PV, PBS.PersistentBlockState PV) -> (AccountAddress, [(Timestamp, Amount)]) -> ThisMonadConcrete ()
checkCorrectAccountReleaseSchedule (blockStateBasic, blockStatePersistent) (oldBlockStateBasic, _) (acc, rel) = do
  let Just newBasicAccount = Concordium.GlobalState.Basic.BlockState.Accounts.getAccount acc (blockStateBasic ^. blockAccounts)
  let Just oldBasicAccount = Concordium.GlobalState.Basic.BlockState.Accounts.getAccount acc (oldBlockStateBasic ^. blockAccounts)
  checkEqualAccountReleaseSchedule (blockStateBasic, blockStatePersistent) acc
  assert (oldBasicAccount ^. accountAmount + sum (map snd rel) == newBasicAccount ^. accountAmount) $ return ()
  let oldReleaseSchedule = oldBasicAccount ^. accountReleaseSchedule
  let newReleaseSchedule = newBasicAccount ^. accountReleaseSchedule
  assert (newReleaseSchedule ^. values == Vector.snoc (oldReleaseSchedule ^. values) (Just (map (uncurry Release) rel, dummyTransactionHash))) $ return ()

-- | Check that the implementations have the same data and that the difference between the old state and the new state after unlocking amounts at the given timestamp is the amount given in the last argument
-- also check that all the timestamps for this account are over the unlocked timestamps.
checkCorrectShrinkingAccountReleaseSchedule :: (BS.BlockState PV, PBS.PersistentBlockState PV) -> (BS.BlockState PV, PBS.PersistentBlockState PV) -> Timestamp -> [(AccountAddress, Amount)] -> ThisMonadConcrete ()
checkCorrectShrinkingAccountReleaseSchedule (blockStateBasic, blockStatePersistent) (oldBlockStateBasic, _) ts accs =
  let f (acc, rel) = do
        let Just newBasicAccount = Concordium.GlobalState.Basic.BlockState.Accounts.getAccount acc (blockStateBasic ^. blockAccounts)
        let Just oldBasicAccount = Concordium.GlobalState.Basic.BlockState.Accounts.getAccount acc (oldBlockStateBasic ^. blockAccounts)
        checkEqualAccountReleaseSchedule (blockStateBasic, blockStatePersistent) acc
        assert (newBasicAccount ^. accountReleaseSchedule . totalLockedUpBalance + rel == oldBasicAccount ^. accountReleaseSchedule . totalLockedUpBalance) $ return ()
        assert (Vector.foldl (\accum -> \case
                                 Nothing -> accum
                                 Just (rels, _) -> sum (map (\(Release _ x) -> x) rels) + accum) 0 (newBasicAccount ^. accountReleaseSchedule . values) == newBasicAccount ^. accountReleaseSchedule . totalLockedUpBalance) $ return ()
        assert (Vector.all (\case
                               Nothing -> True
                               Just (rels, _) -> any (\(Release t _) -> t > ts) rels) (newBasicAccount ^. accountReleaseSchedule . values)) $ return ()
  in
    mapM_ f accs
