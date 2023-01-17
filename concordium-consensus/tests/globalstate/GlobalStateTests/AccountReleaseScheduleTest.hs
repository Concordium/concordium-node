{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module GlobalStateTests.AccountReleaseScheduleTest (tests) where

import Concordium.GlobalState.Account
import Concordium.GlobalState.Basic.BlockState.AccountReleaseSchedule
import qualified Concordium.GlobalState.Basic.BlockState.AccountReleaseScheduleV1 as TARSV1
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.DummyData
import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.GlobalState.Persistent.BlockState as PBS
import Concordium.Scheduler.DummyData
import Concordium.Types
import Control.Monad
import Control.Monad.RWS.Strict as RWS hiding (state)
import Data.Foldable
import Data.List (nub, sort)
import qualified Data.Map.Strict as OrdMap
import Data.Maybe

import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.GlobalState.Persistent.Account (PersistentAccount (..))
import qualified Concordium.GlobalState.Persistent.Account as BS
import Concordium.GlobalState.Persistent.Account.StructureV1
import Concordium.Types.SeedState (initialSeedState)
import Test.HUnit
import Test.Hspec
import Test.QuickCheck

--------------------------------- Monad Types ----------------------------------

-- |Protocol version.
type PV = 'P5

type ThisMonadConcrete pv =
    PBS.PersistentBlockStateMonad
        pv
        (PBS.PersistentBlockStateContext pv)
        (BlobStoreM' (PBS.PersistentBlockStateContext pv))

--------------------------------- Test values ----------------------------------

genTimestamp :: Gen Timestamp
genTimestamp = Timestamp <$> arbitrary

dummyTransactionHash :: TransactionHash
dummyTransactionHash = read "f26a45adbb7d5cbefd9430d1eac665bd225fb3d8e04efb288d99a0347f0b8868"

transactions :: Int -> IO [[(Timestamp, Amount)]]
transactions numOfSchedules = do
    let numReleasesInEach = map getPositive <$> vectorOf numOfSchedules arbitrary
        listsOfTimestamps = fmap (sort . nub . map (`mod` 200)) . flip vectorOf genTimestamp
        listsOfTuples = mapM (flip fmap ((`mod` 200) . getPositive <$> arbitrary) . (,))
    generate $ numReleasesInEach >>= mapM (listsOfTimestamps >=> listsOfTuples)

setOfTransactions :: Int -> IO ([[(Timestamp, Amount)]], OrdMap.Map Timestamp Amount, [Timestamp])
setOfTransactions numTxs = do
    txs <- transactions numTxs
    let amounts = OrdMap.fromListWith (+) . concat $ txs
    let timestamps = OrdMap.keys amounts
    return (txs, amounts, timestamps)

createGS :: ThisMonadConcrete PV (AccountAddress, AccountIndex, AccountAddress, AccountIndex, PBS.PersistentBlockState PV)
createGS = do
    acc0 <- makeTestAccountFromSeed 1_000_000 0
    acc1 <- makeTestAccountFromSeed 1_000_000 1
    initState <-
        PBS.hpbsPointers
            <$> PBS.initialPersistentState
                (initialSeedState (Hash.hash "") 1_000)
                dummyCryptographicParameters
                [acc0, acc1]
                dummyIdentityProviders
                dummyArs
                dummyKeyCollection
                dummyChainParameters
    addr0 <- BS.accountCanonicalAddress acc0
    addr1 <- BS.accountCanonicalAddress acc1
    return (addr0, 0, addr1, 1, initState)

------------------------------------- Test -------------------------------------

testing :: ThisMonadConcrete PV ()
testing = do
    (!txsA, !amountsA, _) <- liftIO $ setOfTransactions 3
    (!txsB, !amountsB, _) <- liftIO $ setOfTransactions 2
    (accA, aiA, accB, aiB, bs) <- createGS
    bs'' <-
        foldlM
            ( \oldState e@((_, ai), rels) -> do
                -- Make a copy of the initial state before modifying it
                -- so that we can compare the state before and after.
                newCopy <- thawBlockState =<< freezeBlockState oldState
                newB <- bsoModifyAccount newCopy ((emptyAccountUpdate ai){_auReleaseSchedule = Just [(rels, dummyTransactionHash)]})
                checkCorrectAccountReleaseSchedule newB oldState e
                return newB
            )
            bs
            (map ((accA, aiA),) txsA ++ map ((accB, aiB),) txsB)
    let taggedAmounts = OrdMap.toAscList $ OrdMap.unionWith (++) (OrdMap.map ((: []) . (accA,)) amountsA) (OrdMap.map ((: []) . (accB,)) amountsB)
    _ <-
        foldlM
            ( \oldState (ts, accs) -> do
                -- Make a copy of the initial state before modifying it
                -- so that we can compare the state before and after.
                newCopy <- thawBlockState =<< freezeBlockState oldState
                newB <- bsoProcessReleaseSchedule newCopy ts
                checkCorrectShrinkingAccountReleaseSchedule newB oldState ts accs
                return newB
            )
            bs''
            taggedAmounts
    return ()

tests :: Spec
tests = do
    describe "GlobalState.AccountReleaseScheduleTest" $
        specify "correct releases" $
            runBlobStoreTemp "." $
                PBS.withNewAccountCache 1_000 $
                    PBS.runPersistentBlockStateMonad testing

------------------------------------ Checks ------------------------------------

-- | Check that an an account was correctly updated in the blockstate with the given release schedule.
checkCorrectAccountReleaseSchedule :: PBS.PersistentBlockState PV -> PBS.PersistentBlockState PV -> ((AccountAddress, AccountIndex), [(Timestamp, Amount)]) -> ThisMonadConcrete PV ()
checkCorrectAccountReleaseSchedule newBlockState oldBlockState ((acc, _), rel) = do
    (_, newAccountState') <- fromJust <$> bsoGetAccount newBlockState acc
    (_, oldAccountState') <- fromJust <$> bsoGetAccount oldBlockState acc
    let newAccountState = case newAccountState' of
            PAV2 a -> a
        oldAccountState = case oldAccountState' of
            PAV2 a -> a
    newReleaseSchedule <- theAccountReleaseScheduleV1 <$> getReleaseSchedule newAccountState
    oldReleaseSchedule <- theAccountReleaseScheduleV1 <$> getReleaseSchedule oldAccountState
    let newAmount = accountAmount newAccountState
    let oldAmount = accountAmount oldAccountState
    liftIO $
        assertEqual
            ("Old amount + sum of new releases == new amount " ++ show oldAmount ++ ", " ++ show rel)
            (oldAmount + sum (map snd rel))
            newAmount
    let testEntry TARSV1.ReleaseScheduleEntry{..} = toList (TARSV1.relReleases rseReleases) == rel
    -- Ensure that the new release schedule contains a new release entry which contains exactly the releases specified.
    let comp b (x : xs) (y : ys)
            | x == y = comp b xs ys
            | otherwise = do
                assertBool "Test release" $ testEntry y
                comp True (x : xs) ys
        comp False [] [y] = assertBool "Release not equal" $ testEntry y
        comp b [] [] = assertBool "New release not found" b
        comp _ left right = assertFailure $ "Lists of unexpected length: " ++ show oldReleaseSchedule ++ ", " ++ show newReleaseSchedule ++ ", " ++ show rel ++ ", " ++ show left ++ ", " ++ show right
    liftIO (comp False (TARSV1.arsReleases oldReleaseSchedule) (TARSV1.arsReleases newReleaseSchedule))

-- | Check that the difference between the old state and the new state after
-- unlocking amounts at the given timestamp is the amount given in the last
-- argument also check that all the timestamps for this account are over the
-- unlocked timestamps.
checkCorrectShrinkingAccountReleaseSchedule :: PBS.PersistentBlockState PV -> PBS.PersistentBlockState PV -> Timestamp -> [(AccountAddress, Amount)] -> ThisMonadConcrete PV ()
checkCorrectShrinkingAccountReleaseSchedule newBlockState oldBlockState ts accs =
    let f (acc, rel) = do
            (_, newAccountState) <- fromJust <$> bsoGetAccount newBlockState acc
            (_, oldAccountState) <- fromJust <$> bsoGetAccount oldBlockState acc
            newLockedAmount <- getAccountLockedAmount newAccountState
            oldLockedAmount <- getAccountLockedAmount oldAccountState
            let newAccountStateInner = case newAccountState of
                    PAV2 a -> a
            liftIO $
                assertEqual
                    "New locked balance + released amount == old locked balance"
                    (newLockedAmount + rel)
                    oldLockedAmount
            newReleases <- TARSV1.arsReleases . theAccountReleaseScheduleV1 <$> getReleaseSchedule newAccountStateInner
            liftIO $
                assertEqual
                    "Sum of releases == locked balance"
                    (foldl' (\accum TARSV1.ReleaseScheduleEntry{..} -> sum (fmap snd (TARSV1.relReleases rseReleases)) + accum) 0 newReleases)
                    newLockedAmount
            liftIO $ assertBool "Remaining releases are after release time" (all (\TARSV1.ReleaseScheduleEntry{..} -> any (\(t, _) -> t > ts) (TARSV1.relReleases rseReleases)) newReleases)
    in  mapM_ f accs
