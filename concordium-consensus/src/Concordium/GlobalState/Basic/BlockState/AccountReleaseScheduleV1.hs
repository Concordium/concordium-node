{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Concordium.GlobalState.Basic.BlockState.AccountReleaseScheduleV1 where

import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Serialize
import qualified Data.Vector as Vector

import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Types

import qualified Concordium.GlobalState.Basic.BlockState.AccountReleaseSchedule as ARSV0
import Concordium.Types.HashableTo
import Data.List (sortOn)

newtype Releases = Releases {relReleases :: NonEmpty (Timestamp, Amount)}

hashReleases :: Releases -> Hash.Hash
hashReleases Releases{..} = hshRels relReleases
  where
    serRel (ts, amt) = put ts >> put amt
    hshRels (rel :| []) = Hash.hashLazy . runPutLazy $ serRel rel
    hshRels (rel :| (h : t)) = Hash.hashLazy . runPutLazy $ serRel rel >> put (hshRels (h :| t))

data ReleaseScheduleEntry = ReleaseScheduleEntry
    { rseReleases :: !Releases,
      rseReleasesHash :: !Hash.Hash,
      rseTransactionHash :: !TransactionHash
    }

rseNextTimestamp :: ReleaseScheduleEntry -> Timestamp
rseNextTimestamp = fst . NE.head . relReleases . rseReleases

rseSortKey :: ReleaseScheduleEntry -> (Timestamp, Hash.Hash)
rseSortKey rse = (rseNextTimestamp rse, rseReleasesHash rse)

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

newtype AccountReleaseSchedule = AccountReleaseSchedule
    { -- |The release entries ordered on 'rseSortKey'.
      arsReleases :: [ReleaseScheduleEntry]
    }

newtype AccountReleaseScheduleHashV1 = AccountReleaseScheduleHashV1
    { theAccountReleaseScheduleHashV1 :: Hash.Hash
    }
    deriving (Eq, Ord, Show, Serialize)

emptyAccountReleaseScheduleHashV1 :: AccountReleaseScheduleHashV1
emptyAccountReleaseScheduleHashV1 = AccountReleaseScheduleHashV1 (Hash.hash "")

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
emptyAccountReleaseSchedule = AccountReleaseSchedule []

-- | Returns 'True' if the account release schedule contains no releases.
isEmptyAccountReleaseSchedule :: AccountReleaseSchedule -> Bool
isEmptyAccountReleaseSchedule = null . arsReleases

-- |Get the timestamp at which the next scheduled release will occur (if any).
nextReleaseTimestamp :: AccountReleaseSchedule -> Maybe Timestamp
nextReleaseTimestamp ars = case arsReleases ars of
    [] -> Nothing
    (h : _) -> Just $! rseNextTimestamp h

insertEntry :: ReleaseScheduleEntry -> AccountReleaseSchedule -> AccountReleaseSchedule
insertEntry entry ars = AccountReleaseSchedule (ins (arsReleases ars))
  where
    ins [] = [entry]
    ins (h : t)
        | rseSortKey h < rseSortKey entry = entry : h : t
        | otherwise = h : ins t

-- | Insert a new schedule in the structure.
--
-- Precondition: The given list of timestamps and amounts MUST NOT be empty.
addReleases :: ([(Timestamp, Amount)], TransactionHash) -> AccountReleaseSchedule -> AccountReleaseSchedule
addReleases (h : t, rseTransactionHash) = insertEntry entry
  where
    rseReleases = Releases (h :| t)
    rseReleasesHash = hashReleases rseReleases
    entry = ReleaseScheduleEntry{..}
addReleases _ = error "addReleases: Empty list of timestamps and amounts."

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
    (!relAmt, !newArs) = foldr updateEntry (0, AccountReleaseSchedule staticReleases) elapsedReleases

fromAccountReleaseScheduleV0 :: ARSV0.AccountReleaseSchedule -> AccountReleaseSchedule
fromAccountReleaseScheduleV0 ARSV0.AccountReleaseSchedule{..} = AccountReleaseSchedule newReleases
  where
    pendRels = Map.toList _pendingReleases
    mkEntry i = case _values Vector.! i of
        Just (r0 : rs, th) ->
            ReleaseScheduleEntry
                { rseReleases = rels,
                  rseReleasesHash = hashReleases rels,
                  rseTransactionHash = th
                }
          where
            rels = Releases $ fmap (\(ARSV0.Release a b) -> (a, b)) (r0 :| rs)
        _ -> error "fromAccountReleaseScheduleV0: missing release"
    mkEntries (_, is) = map mkEntry is
    newReleases = sortOn rseSortKey $ concatMap mkEntries pendRels