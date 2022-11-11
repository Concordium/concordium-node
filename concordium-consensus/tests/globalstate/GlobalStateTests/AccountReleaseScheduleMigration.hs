{-# LANGUAGE BangPatterns #-}

module GlobalStateTests.AccountReleaseScheduleMigration where

import Control.Monad.IO.Class
import Control.Monad.Loops
import qualified Data.ByteString as BS
import Data.Function
import Data.List (sort)
import Lens.Micro.Platform
import Test.HUnit
import Test.Hspec
import Test.QuickCheck

import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Types
import Concordium.Types.HashableTo

import qualified Concordium.GlobalState.Basic.BlockState.AccountReleaseScheduleV0 as TARSV0
import qualified Concordium.GlobalState.Basic.BlockState.AccountReleaseScheduleV1 as TARSV1
import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.GlobalState.Persistent.BlockState.AccountReleaseSchedule as PARSV0
import qualified Concordium.GlobalState.Persistent.BlockState.AccountReleaseScheduleV1 as PARSV1

th1, th2, th3 :: TransactionHash
th1 = read "0000000000000000000000000000000000000000000000000000000000000001"
th2 = read "0000000000000000000000000000000000000000000000000000000000000002"
th3 = read "0000000000000000000000000000000000000000000000000000000000000003"

-- |A V0 transient account release schedule after adding some releases and then unlocking.
dummyTARSV0 :: TARSV0.AccountReleaseSchedule
dummyTARSV0 =
    TARSV0.emptyAccountReleaseSchedule
        & TARSV0.addReleases ([(10, 10), (20, 10), (30, 10), (40, 10)], th1)
        & TARSV0.addReleases ([(5, 10), (10, 10), (15, 10), (20, 10)], th2)
        & TARSV0.addReleases ([(25, 10), (30, 10), (35, 10), (40, 10)], th3)
        & view _3 . TARSV0.unlockAmountsUntil 20

-- |A V1 transient account release schedule created by the same operations as 'dummyTARSV0'.
dummyTARSV1 :: TARSV1.AccountReleaseSchedule
dummyTARSV1 =
    TARSV1.emptyAccountReleaseSchedule
        & TARSV1.addReleases ([(10, 10), (20, 10), (30, 10), (40, 10)], th1)
        & TARSV1.addReleases ([(5, 10), (10, 10), (15, 10), (20, 10)], th2)
        & TARSV1.addReleases ([(25, 10), (30, 10), (35, 10), (40, 10)], th3)
        & view _3 . TARSV1.unlockAmountsUntil 20

-- |Construct a V0 persistent release schedule, save and load it under a reference, migrate it
-- to V1, extract the transient release schedule, and test it against 'dummyTARSV1', which should
-- match. This also tests that extracting the transient release schedule before migration gives
-- the same result as 'dummyTARSV0'.
testMigrate :: IO ()
testMigrate = do
    mbs0 <- newMemBlobStore
    flip runMemBlobStoreT mbs0 $ do
        rs0 <-
            PARSV0.emptyAccountReleaseSchedule
                & PARSV0.addReleases ([(10, 10), (20, 10), (30, 10), (40, 10)], th1)
                >>= PARSV0.addReleases ([(5, 10), (10, 10), (15, 10), (20, 10)], th2)
                >>= PARSV0.addReleases ([(25, 10), (30, 10), (35, 10), (40, 10)], th3)
                >>= fmap (view _3) . PARSV0.unlockAmountsUntil 20
        rs0Ref <- makeUnbufferedRef rs0
        rs0' <- refLoad rs0Ref
        trs0 <- PARSV0.loadPersistentAccountReleaseSchedule rs0'
        liftIO $ assertEqual "V0 release schedule" dummyTARSV0 trs0
        mbs1 <- liftIO newMemBlobStore
        flip runMemBlobStoreT mbs1 $ do
            rs1 <- PARSV1.migrateAccountReleaseScheduleFromV0 rs0'
            trs1 <- PARSV1.getAccountReleaseSchedule (PARSV0.releaseScheduleLockedBalance rs0') rs1
            liftIO $ assertEqual "V1 release schedule" dummyTARSV1 trs1

-- |Operations on an account release schedule. These are used for generating random test cases.
data ARSOp
    = AddReleases ([(Timestamp, Amount)], TransactionHash)
    | Unlock Timestamp
    deriving (Show)

instance Arbitrary ARSOp where
    arbitrary = oneof [addReleases, unlock]
      where
        addReleases = do
            rels <-
                sort
                    <$> listOf1
                        ( do
                            ts <- Timestamp <$> arbitrary
                            -- We limit the size of the amounts we consider to avoid overflow.
                            amt <- Amount <$> chooseBoundedIntegral (0, 1000000000)
                            return (ts, amt)
                        )
            th <- TransactionHashV0 . Hash.hash . BS.pack <$> arbitrary
            return $ AddReleases (rels, th)
        unlock = Unlock . Timestamp <$> arbitrary

-- |Execute an 'ARSOp' in the persistent V0 account release schedule.
execARSOpPV0 :: (MonadBlobStore m) => ARSOp -> PARSV0.AccountReleaseSchedule -> m PARSV0.AccountReleaseSchedule
execARSOpPV0 (AddReleases rel) = PARSV0.addReleases rel
execARSOpPV0 (Unlock ts) = fmap (view _3) . PARSV0.unlockAmountsUntil ts

-- |Execute an 'ARSOp' in the persistent V1 account release schedule. This also takes in and
-- returns the total locked amount in the release schedule.
execARSOpPV1 :: (MonadBlobStore m) => ARSOp -> (PARSV1.AccountReleaseSchedule, Amount) -> m (PARSV1.AccountReleaseSchedule, Amount)
execARSOpPV1 (AddReleases rel) (rs, lockedAmt) = do
    rs' <- PARSV1.addReleases rel rs
    let !lockedAmt' = lockedAmt + sum (snd <$> fst rel)
    return (rs', lockedAmt')
execARSOpPV1 (Unlock ts) (rs, lockedAmt) = do
    (relAmt, _, rs') <- PARSV1.unlockAmountsUntil ts rs
    liftIO $ assertBool "Release amount should be no more than locked amount" (relAmt <= lockedAmt)
    return (rs', lockedAmt - relAmt)

-- |This test takes a sequence of account release schedule operations and performs them on the
-- V0 and V1 persistent account release schedule implementations. It checks that the locked balances
-- at the end match. It also migrates the V0 ARS to the V1, and checks that the hash matches the
-- non-migrated V1 ARS. It also checks that the hashes are preserved for the V0 and V1 versions
-- when they are extracted to the transient versions of the account release schedule.
--
-- Note, we do not extract the transient schedule from both V1 versions and check equality since
-- there are cases in which they may permissibly differ. In particular, if there are identical
-- entries with differing transaction hashes, then the order of the entries is arbitrary. This is
-- allowed because the transaction hashes do not contribute to the overall hash of the release
-- schedule.
testOperations :: [ARSOp] -> Property
testOperations ops = ioProperty $ do
    mbs0 <- newMemBlobStore
    (ars1h, amt1) <- flip runMemBlobStoreT mbs0 $ do
        rs0 <- concatM (execARSOpPV0 <$> ops) PARSV0.emptyAccountReleaseSchedule
        h0 <- getHashM rs0
        trs <- PARSV0.loadPersistentAccountReleaseSchedule rs0
        liftIO $ assertEqual "V0 hash and transient hash" (getHash trs :: TARSV0.AccountReleaseScheduleHashV0) h0
        mbst <- liftIO newMemBlobStore
        h <- flip runMemBlobStoreT mbst $ do
            mrs <- PARSV1.migrateAccountReleaseScheduleFromV0 rs0
            getHashM mrs
        return (h, PARSV0.releaseScheduleLockedBalance rs0)
    mbs1 <- newMemBlobStore
    (ars2h, amt2) <- flip runMemBlobStoreT mbs1 $ do
        (rs1, amt) <- concatM (execARSOpPV1 <$> ops) (PARSV1.emptyAccountReleaseSchedule, 0)
        h <- getHashM rs1
        trs <- PARSV1.getAccountReleaseSchedule amt rs1
        liftIO $ assertEqual "V1 hash and transient hash" (getHash trs) h
        return (h, amt)
    return $
        (amt1 === amt2)
            .&&. ((ars1h :: TARSV1.AccountReleaseScheduleHashV1) === ars2h)

tests :: Spec
tests = describe "GlobalState.AccountReleaseScheduleMigration" $ do
    specify "Transient: schedule after migration is as expected" $
        assertEqual
            "V1 summary and migrated V0 summary"
            dummyTARSV1
            (TARSV1.fromAccountReleaseScheduleV0 dummyTARSV0)
    specify "Migration" testMigrate
    specify "OperationsV0;migration;(hash,lockedAmt) ~ OperationsV1;(hash,lockedAmt)" $ withMaxSuccess 10000 testOperations