{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Concordium.GlobalState.Basic.BlockState.AccountReleaseScheduleV1 where

import Control.Monad
import Data.Foldable
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Serialize
import qualified Data.Vector as Vector

import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Types
import Concordium.Types.Accounts.Releases
import Concordium.Types.HashableTo
import Concordium.Utils.Serialization

import qualified Concordium.GlobalState.Basic.BlockState.AccountReleaseScheduleV0 as ARSV0

newtype Releases = Releases {relReleases :: NonEmpty (Timestamp, Amount)}
    deriving (Eq, Show)

instance Serialize Releases where
    put Releases{..} = do
        putLength (NE.length relReleases)
        mapM_ put relReleases
    get = do
        len <- getLength
        if len < 1
            then fail "Releases must me non-empty"
            else do
                h <- get
                t <- replicateM (len - 1) get
                return $! Releases (h :| t)

hashReleases :: Releases -> Hash.Hash
hashReleases Releases{..} = hshRels relReleases
  where
    serRel (ts, amt) = put ts >> put amt
    hshRels (rel :| []) = Hash.hashLazy . runPutLazy $ serRel rel
    hshRels (rel :| (h : t)) = Hash.hashLazy . runPutLazy $ serRel rel >> put (hshRels (h :| t))

-- |A release schedule produced by a single scheduled transfer. An account can
-- have any number (including 0) of release schedule entries.
data ReleaseScheduleEntry = ReleaseScheduleEntry
    { rseReleases :: !Releases,
      rseReleasesHash :: !Hash.Hash,
      rseTransactionHash :: !TransactionHash
    }
    deriving (Eq, Show)

instance Serialize ReleaseScheduleEntry where
    put ReleaseScheduleEntry{..} = do
        put rseTransactionHash
        put rseReleases
    get = do
        rseTransactionHash <- get
        rseReleases <- get
        let rseReleasesHash = hashReleases rseReleases
        return $! ReleaseScheduleEntry{..}

-- |Timestamp of the earliest release in the given 'ReleaseScheduleEntry'
rseNextTimestamp :: ReleaseScheduleEntry -> Timestamp
rseNextTimestamp = fst . NE.head . relReleases . rseReleases

-- |The key used to order release schedule entries. An account has a list of
-- 'ReleaseScheduleEntry', and they are maintained ordered by this key. The
-- ordering is first by timestamp, and ties are resolved by the hash of the
-- releases.
rseSortKey :: ReleaseScheduleEntry -> (Timestamp, Hash.Hash)
rseSortKey rse = (rseNextTimestamp rse, rseReleasesHash rse)

-- |Unlock releases on the given 'ReleaseScheduleEntry' up to, and including,
-- the given timestamp. The unlocked amount is returned, as well as potentially
-- the remaining release schedule if any releases are remaining.
unlockEntryUntil :: Timestamp -> ReleaseScheduleEntry -> (Amount, Maybe ReleaseScheduleEntry)
unlockEntryUntil ts ReleaseScheduleEntry{..} = (sum (snd <$> lapsed), mrse)
  where
    (lapsed, remain) = NE.break (\(t, _) -> ts < t) (relReleases rseReleases)
    mrse = case remain of
        [] -> Nothing
        (h : t) ->
            Just
                ReleaseScheduleEntry
                    { rseReleases = newReleases,
                      rseReleasesHash = hashReleases newReleases,
                      ..
                    }
          where
            newReleases = Releases (h :| t)

data AccountReleaseSchedule = AccountReleaseSchedule
    { -- |The release entries ordered on 'rseSortKey'.
      arsReleases :: ![ReleaseScheduleEntry],
      -- |The total locked amount of all releases.
      arsTotalLockedAmount :: !Amount
    }
    deriving (Eq, Show)

instance Serialize AccountReleaseSchedule where
    put AccountReleaseSchedule{..} = do
        putLength (length arsReleases)
        mapM_ put arsReleases
    get = do
        relsLength <- getLength
        arsReleases <- replicateM relsLength get
        let arsTotalLockedAmount = sum [sum (snd <$> relReleases (rseReleases rse)) | rse <- arsReleases]
        return $! AccountReleaseSchedule{..}

newtype AccountReleaseScheduleHashV1 = AccountReleaseScheduleHashV1
    { theAccountReleaseScheduleHashV1 :: Hash.Hash
    }
    deriving (Eq, Ord, Show, Serialize)

emptyAccountReleaseScheduleHashV1 :: AccountReleaseScheduleHashV1
emptyAccountReleaseScheduleHashV1 = AccountReleaseScheduleHashV1 (Hash.hash "EmptyAccountReleaseScheduleV1")

consAccountReleaseScheduleHashV1 :: Hash.Hash -> AccountReleaseScheduleHashV1 -> AccountReleaseScheduleHashV1
consAccountReleaseScheduleHashV1 h arsh = AccountReleaseScheduleHashV1 . Hash.hashLazy . runPutLazy $ put h >> put arsh

instance HashableTo AccountReleaseScheduleHashV1 AccountReleaseSchedule where
    getHash AccountReleaseSchedule{..} =
        foldr
            (consAccountReleaseScheduleHashV1 . rseReleasesHash)
            emptyAccountReleaseScheduleHashV1
            arsReleases

-- | The empty account release schedule.
emptyAccountReleaseSchedule :: AccountReleaseSchedule
emptyAccountReleaseSchedule = AccountReleaseSchedule [] 0

-- | Returns 'True' if the account release schedule contains no releases.
isEmptyAccountReleaseSchedule :: AccountReleaseSchedule -> Bool
isEmptyAccountReleaseSchedule = null . arsReleases

-- |Get the timestamp at which the next scheduled release will occur (if any).
nextReleaseTimestamp :: AccountReleaseSchedule -> Maybe Timestamp
nextReleaseTimestamp ars = case arsReleases ars of
    [] -> Nothing
    (h : _) -> Just $! rseNextTimestamp h

-- |Insert an entry in an ordered list of entries at the first point that preserves the ordering.
-- The ordering is defined by 'rseSortKey'.
insertEntry :: ReleaseScheduleEntry -> [ReleaseScheduleEntry] -> [ReleaseScheduleEntry]
insertEntry entry = ins
  where
    ins [] = [entry]
    ins (h : t)
        | rseSortKey entry < rseSortKey h = entry : h : t
        | otherwise = h : ins t

-- | Insert a new schedule in the structure.
--
-- Precondition: The given list of timestamps and amounts MUST NOT be empty. Moreover, the
-- timestamps MUST be in ascending order.
addReleases :: ([(Timestamp, Amount)], TransactionHash) -> AccountReleaseSchedule -> AccountReleaseSchedule
addReleases (l@(h : t), rseTransactionHash) AccountReleaseSchedule{..} =
    AccountReleaseSchedule (insertEntry entry arsReleases) (arsTotalLockedAmount + sum (snd <$> l))
  where
    rseReleases = Releases (h :| t)
    rseReleasesHash = hashReleases rseReleases
    entry = ReleaseScheduleEntry{..}
addReleases _ _ = error "addReleases: Empty list of timestamps and amounts."

-- | Returns the amount that was unlocked, the next timestamp for this account
-- (if there is one) and the new account release schedule after removing the
-- amounts whose timestamp was less or equal to the given timestamp.
unlockAmountsUntil :: Timestamp -> AccountReleaseSchedule -> (Amount, Maybe Timestamp, AccountReleaseSchedule)
unlockAmountsUntil ts ars0 = (relAmt, nextReleaseTimestamp newArs, newArs)
  where
    (elapsedReleases, staticReleases) = break (\r -> ts < rseNextTimestamp r) (arsReleases ars0)
    updateEntry e (!accumAmt, !ars) = case unlockEntryUntil ts e of
        (!am, Just !e') -> (am + accumAmt, insertEntry e' ars)
        (!am, Nothing) -> (am + accumAmt, ars)
    (!relAmt, !newEntries) = foldr updateEntry (0, staticReleases) elapsedReleases
    newArs = AccountReleaseSchedule newEntries (arsTotalLockedAmount ars0 - relAmt)

-- |Migrate a V0 'ARSV0.AccountReleaseSchedule' to a V1 'ARSV1.AccountReleaseSchedule'.
fromAccountReleaseScheduleV0 :: ARSV0.AccountReleaseSchedule -> AccountReleaseSchedule
fromAccountReleaseScheduleV0 ARSV0.AccountReleaseSchedule{..} = AccountReleaseSchedule newReleases _totalLockedUpBalance
  where
    pendRels = catMaybes $ Vector.toList _values
    mkEntry (r0 : rs, th) =
        ReleaseScheduleEntry
            { rseReleases = rels,
              rseReleasesHash = hashReleases rels,
              rseTransactionHash = th
            }
      where
        rels = Releases $ fmap (\(ARSV0.Release a b) -> (a, b)) (r0 :| rs)
    mkEntry _ = error "fromAccountReleaseScheduleV0: missing release"
    newReleases = sortOn rseSortKey $ mkEntry <$> pendRels

-- |Produce an 'AccountReleaseSummary' from an 'AccountReleaseSchedule'.
toAccountReleaseSummary :: AccountReleaseSchedule -> AccountReleaseSummary
toAccountReleaseSummary AccountReleaseSchedule{..} = AccountReleaseSummary{..}
  where
    agg (amt1, ths1) (amt2, ths2) = (amt, ths)
      where
        !amt = amt1 + amt2
        ths = ths1 ++ ths2
    processRelease th (!m, !acc) (ts, amt) = (Map.insertWith agg ts (amt, [th]) m, acc + amt)
    processEntry acc ReleaseScheduleEntry{..} = foldl' (processRelease rseTransactionHash) acc (relReleases rseReleases)
    releaseMap :: Map.Map Timestamp (Amount, [TransactionHash])
    (releaseMap, releaseTotal) = foldl' processEntry (Map.empty, 0) arsReleases
    makeSR (releaseTimestamp, (releaseAmount, releaseTransactions)) = ScheduledRelease{..}
    releaseSchedule = makeSR <$> Map.toList releaseMap

-- |Compute the sum of releases in the release schedule.
-- This should produce the same result as 'arsTotalLockedAmount', and is provided for testing
-- purposes.
sumOfReleases :: AccountReleaseSchedule -> Amount
sumOfReleases ars = sum [sum (snd <$> relReleases (rseReleases rse)) | rse <- arsReleases ars]
