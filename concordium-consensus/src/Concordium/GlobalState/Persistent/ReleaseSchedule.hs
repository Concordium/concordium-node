{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.GlobalState.Persistent.ReleaseSchedule where

import Control.Arrow
import Control.Monad
import Data.Map.Strict as Map
import Data.Serialize
import Data.Set as Set
import Data.Word

import Concordium.Types

import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.GlobalState.Persistent.Trie as Trie
import Concordium.Utils

class ReleaseScheduleOperations m r where
    -- |A reference to an account as handled by the release schedule.
    type AccountRef r

    -- |Add a release for a given account.
    -- PRECONDITION: The account must not already have a scheduled release.
    addAccountRelease :: Timestamp -> AccountRef r -> r -> m r

    -- |Update the scheduled release time for an account.
    -- PRECONDITION: The account must have a scheduled release at the old release time.
    -- PRECONDITION: @newReleaseTime <= oldReleaseTime@.
    updateAccountRelease ::
        -- |Old release time
        Timestamp ->
        -- |New release time
        Timestamp ->
        AccountRef r ->
        r ->
        m r

    -- |Remove all releases from the schedule up to and including the provided timestamp,
    -- returning a list of the accounts with removed releases.
    processReleasesUntil :: Timestamp -> r -> m ([AccountRef r], r)

-- |A release schedule implementation that is designed to be serialization-compatible with
-- the old representation of the top-level release schedule, which was a map from account
-- addresses to release timestamps.
data LegacyReleaseSchedule = LegacyReleaseSchedule
    { -- |The first timestamp at which a release is scheduled (or the maximum possible timestamp if
      -- no releases are scheduled).
      rsFirstTimestamp :: !Timestamp,
      -- |A map recording the first release time for each account with a pending release.
      -- An account should occur at most once in the map.
      rsMap :: !(Map.Map Timestamp (Set.Set AccountAddress)),
      -- |The number of accounts with entries in the release schedule map.
      rsEntryCount :: !Word64
    }

instance Serialize LegacyReleaseSchedule where
    put LegacyReleaseSchedule{..} = do
        putWord64be rsEntryCount
        forM_ (Map.toAscList rsMap) $ \(ts, accs) ->
            forM_ accs $ \acc -> put acc >> put ts
    get = do
        rsEntryCount <- getWord64be
        (rsMap, rsFirstTimestamp) <- go rsEntryCount Map.empty (Timestamp maxBound)
        return LegacyReleaseSchedule{..}
      where
        go 0 !m !fts = return (m, fts)
        go n !m !fts = do
            acc <- get
            ts <- get
            let addAcc Nothing = Just (Set.singleton acc)
                addAcc (Just accs) = Just $! Set.insert acc accs
            go (n - 1) (Map.alter addAcc ts m) (min ts fts)

instance MonadBlobStore m => BlobStorable m LegacyReleaseSchedule

instance (MonadBlobStore m) => ReleaseScheduleOperations m (BufferedRef LegacyReleaseSchedule) where
    type AccountRef (BufferedRef LegacyReleaseSchedule) = AccountAddress

    addAccountRelease ts addr br = do
        LegacyReleaseSchedule{..} <- refLoad br
        refMake $!
            LegacyReleaseSchedule
                { rsFirstTimestamp = min rsFirstTimestamp ts,
                  rsMap = Map.alter addAcc ts rsMap,
                  rsEntryCount = 1 + rsEntryCount
                }
      where
        addAcc Nothing = Just $! Set.singleton addr
        addAcc (Just accs) = Just $! Set.insert addr accs

    updateAccountRelease oldts ts addr br = do
        LegacyReleaseSchedule{..} <- refLoad br
        let remMap = Map.alter remAcc oldts rsMap
        refMake $!
            LegacyReleaseSchedule
                { -- Since the new timestamp must be less than or equal to the old, this is correct.
                  rsFirstTimestamp = min rsFirstTimestamp ts,
                  rsMap = Map.alter addAcc ts remMap,
                  rsEntryCount = rsEntryCount
                }
      where
        remAcc Nothing = error "updateAccountRelease: no entry at expected release time"
        remAcc (Just s) =
            let s' = Set.delete addr s
             in if Set.null s' then Nothing else Just s'
        addAcc Nothing = Just (Set.singleton addr)
        addAcc (Just accs) = Just $! Set.insert addr accs

    processReleasesUntil ts br = do
        rs@LegacyReleaseSchedule{..} <- refLoad br
        if ts < rsFirstTimestamp
            then return ([], br)
            else do
                let (newRS, accs) = go rs []
                (accs,) <$> refMake newRS
      where
        go rs@LegacyReleaseSchedule{..} accum
            | Map.null rsMap = (LegacyReleaseSchedule (Timestamp maxBound) Map.empty 0, accum)
            | minTS <= ts =
                go
                    (rs{rsMap = newMap, rsEntryCount = rsEntryCount - fromIntegral (Set.size accs)})
                    (accum ++ Set.toList accs)
            | otherwise = (rs{rsFirstTimestamp = minTS}, accum)
          where
            -- Pattern match lazily in case the map is empty.
            ~((minTS, accs), newMap) = Map.deleteFindMin rsMap

newtype AccountSet = AccountSet {theAccountSet :: Set.Set AccountIndex}
    deriving (Serialize)

instance MonadBlobStore m => BlobStorable m AccountSet
instance Applicative m => Cacheable m AccountSet

-- |A release schedule for the P5 protocol version.  This uses a 'Trie.Trie' mapping 'Timestamp's
-- to sets of accounts.
data NewReleaseSchedule = NewReleaseSchedule
    { -- |The first timestamp at which a release is scheduled (or the maximum possible timestamp if
      -- no releases are scheduled).
      rs5FirstTimestamp :: !Timestamp,
      -- |A map recording the first release time for each account with a pending release.
      -- An account should occur at most once in the map.
      rs5Map :: !(Trie.TrieN BufferedFix Timestamp AccountSet)
    }

instance MonadBlobStore m => BlobStorable m NewReleaseSchedule where
    storeUpdate NewReleaseSchedule{..} = do
        (pmap, newMap) <- storeUpdate rs5Map
        let !p = do
                put rs5FirstTimestamp
                pmap
        let !rs = NewReleaseSchedule{rs5Map = newMap, ..}
        return (p, rs)
    load = do
        rs5FirstTimestamp <- get
        mmap <- load
        return $! do
            rs5Map <- mmap
            return $! NewReleaseSchedule{..}

instance (MonadBlobStore m) => ReleaseScheduleOperations m NewReleaseSchedule where
    type AccountRef NewReleaseSchedule = AccountIndex

    addAccountRelease ts ai rs = do
        (_, rs5Map) <- Trie.adjust addAcc ts (rs5Map rs)
        return $!
            NewReleaseSchedule
                { rs5FirstTimestamp = min ts (rs5FirstTimestamp rs),
                  ..
                }
      where
        addAcc Nothing = return ((), Trie.Insert (AccountSet (Set.singleton ai)))
        addAcc (Just (AccountSet accs)) = return $!! ((), Trie.Insert (AccountSet (Set.insert ai accs)))

    updateAccountRelease oldts newts ai rs = do
        (_, rsRem) <- Trie.adjust remAcc oldts (rs5Map rs)
        (_, rs5Map) <- Trie.adjust addAcc newts rsRem
        return $!
            NewReleaseSchedule
                { rs5FirstTimestamp = min newts (rs5FirstTimestamp rs),
                  ..
                }
      where
        remAcc Nothing = error "updateAccountRelease: no entry at expected release time"
        remAcc (Just (AccountSet accs)) =
            return $!
                let accs' = Set.delete ai accs
                 in if Set.null accs' then ((), Trie.Remove) else ((), Trie.Insert (AccountSet accs'))
        addAcc Nothing = return ((), Trie.Insert (AccountSet (Set.singleton ai)))
        addAcc (Just (AccountSet accs)) = return $!! ((), Trie.Insert (AccountSet (Set.insert ai accs)))

    processReleasesUntil ts rs = do
        if ts < rs5FirstTimestamp rs
            then return ([], rs)
            else go [] (rs5Map rs)
      where
        go accum m =
            Trie.findMin m >>= \case
                Nothing -> return $!! (accum, NewReleaseSchedule (Timestamp maxBound) m)
                Just (minTS, accs)
                    | ts < minTS -> return $!! (accum, NewReleaseSchedule minTS m)
                    | otherwise -> do
                        newMap <- Trie.delete minTS m
                        go (accum ++ Set.toList (theAccountSet accs)) newMap

data ReleaseSchedule pv where
    ReleaseScheduleP0 ::
        (RSAccountRef pv ~ AccountAddress) =>
        !(BufferedRef LegacyReleaseSchedule) ->
        ReleaseSchedule pv
    ReleaseScheduleP5 ::
        (RSAccountRef pv ~ AccountIndex) =>
        !NewReleaseSchedule ->
        ReleaseSchedule pv

type family RSAccountRef pv where
    RSAccountRef 'P1 = AccountAddress
    RSAccountRef 'P2 = AccountAddress
    RSAccountRef 'P3 = AccountAddress
    RSAccountRef 'P4 = AccountAddress
    RSAccountRef _ = AccountIndex

instance (MonadBlobStore m) => ReleaseScheduleOperations m (ReleaseSchedule pv) where
    type AccountRef (ReleaseSchedule pv) = RSAccountRef pv
    addAccountRelease ts addr (ReleaseScheduleP0 rs) = ReleaseScheduleP0 <$!> addAccountRelease ts addr rs
    addAccountRelease ts addr (ReleaseScheduleP5 rs) = ReleaseScheduleP5 <$!> addAccountRelease ts addr rs
    updateAccountRelease oldts ts addr (ReleaseScheduleP0 rs) = ReleaseScheduleP0 <$!> updateAccountRelease oldts ts addr rs
    updateAccountRelease oldts ts addr (ReleaseScheduleP5 rs) = ReleaseScheduleP5 <$!> updateAccountRelease oldts ts addr rs
    processReleasesUntil ts (ReleaseScheduleP0 rs) = second ReleaseScheduleP0 <$!> processReleasesUntil ts rs
    processReleasesUntil ts (ReleaseScheduleP5 rs) = second ReleaseScheduleP5 <$!> processReleasesUntil ts rs

-- makePersistentReleaseSchedule :: (Map.Map Account)

-- ReleaseScheduleP5