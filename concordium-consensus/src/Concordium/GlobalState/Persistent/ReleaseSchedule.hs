{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.GlobalState.Persistent.ReleaseSchedule where

import Control.Arrow
import Control.Monad
import Control.Monad.Trans
import Data.Map.Strict as Map
import Data.Serialize
import Data.Set as Set
import Data.Word

import Concordium.Types
import Concordium.Utils

import qualified Concordium.GlobalState.Basic.BlockState.ReleaseSchedule as Transient
import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.GlobalState.Persistent.Trie as Trie

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
      -- no releases are scheduled). This MUST NOT be used to infer that the release schedule is
      -- empty, since there can be a release at the maximum timestamp.
      lrsFirstTimestamp :: !Timestamp,
      -- |A map recording the first release time for each account with a pending release.
      -- An account should occur at most once in the map.
      lrsMap :: !(Map.Map Timestamp (Set.Set AccountAddress)),
      -- |The number of accounts with entries in the release schedule map.
      lrsEntryCount :: !Word64
    }
    deriving (Show)

instance Serialize LegacyReleaseSchedule where
    put LegacyReleaseSchedule{..} = do
        putWord64be lrsEntryCount
        forM_ (Map.toAscList lrsMap) $ \(ts, accs) ->
            forM_ accs $ \acc -> put acc >> put ts
    get = do
        lrsEntryCount <- getWord64be
        (lrsMap, lrsFirstTimestamp) <- go lrsEntryCount Map.empty (Timestamp maxBound)
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
instance Applicative m => Cacheable m LegacyReleaseSchedule

instance (MonadBlobStore m) => ReleaseScheduleOperations m (BufferedRef LegacyReleaseSchedule) where
    type AccountRef (BufferedRef LegacyReleaseSchedule) = AccountAddress

    addAccountRelease ts addr br = do
        LegacyReleaseSchedule{..} <- refLoad br
        refMake $!
            LegacyReleaseSchedule
                { lrsFirstTimestamp = min lrsFirstTimestamp ts,
                  lrsMap = Map.alter addAcc ts lrsMap,
                  lrsEntryCount = 1 + lrsEntryCount
                }
      where
        addAcc Nothing = Just $! Set.singleton addr
        addAcc (Just accs) = Just $! Set.insert addr accs

    updateAccountRelease oldts ts addr br = do
        LegacyReleaseSchedule{..} <- refLoad br
        let remMap = Map.alter remAcc oldts lrsMap
        refMake $!
            LegacyReleaseSchedule
                { -- Since the new timestamp must be less than or equal to the old, this is correct.
                  lrsFirstTimestamp = min lrsFirstTimestamp ts,
                  lrsMap = Map.alter addAcc ts remMap,
                  lrsEntryCount = lrsEntryCount
                }
      where
        remAcc Nothing = error "updateAccountRelease: no entry at expected release time"
        remAcc (Just s) =
            let s' = Set.delete addr s
            in  if Set.null s' then Nothing else Just s'
        addAcc Nothing = Just (Set.singleton addr)
        addAcc (Just accs) = Just $! Set.insert addr accs

    processReleasesUntil ts br = do
        rs@LegacyReleaseSchedule{..} <- refLoad br
        if ts < lrsFirstTimestamp
            then return ([], br)
            else do
                let (newRS, accs) = go rs []
                (accs,) <$> refMake newRS
      where
        go rs@LegacyReleaseSchedule{..} accum
            | Map.null lrsMap = (LegacyReleaseSchedule (Timestamp maxBound) Map.empty 0, accum)
            | minTS <= ts =
                go
                    (rs{lrsMap = newMap, lrsEntryCount = lrsEntryCount - fromIntegral (Set.size accs)})
                    (accum ++ Set.toList accs)
            | otherwise = (rs{lrsFirstTimestamp = minTS}, accum)
          where
            -- Pattern match lazily in case the map is empty.
            ~((minTS, accs), newMap) = Map.deleteFindMin lrsMap

-- |A set of accounts represented by 'AccountIndex'.
newtype AccountSet = AccountSet {theAccountSet :: Set.Set AccountIndex}
    deriving (Serialize, Show)

instance MonadBlobStore m => BlobStorable m AccountSet
instance Applicative m => Cacheable m AccountSet

-- |A release schedule for the P5 protocol version.  This uses a 'Trie.Trie' mapping 'Timestamp's
-- to sets of accounts.
data NewReleaseSchedule = NewReleaseSchedule
    { -- |The first timestamp at which a release is scheduled (or the maximum possible timestamp if
      -- no releases are scheduled). This MUST NOT be used to infer that the release schedule is
      -- empty, since there can be a release at the maximum timestamp.
      nrsFirstTimestamp :: !Timestamp,
      -- |A map recording the first release time for each account with a pending
      -- release. An account should occur at most once in the map. We make
      -- crucial use of the lexicographic ordering on the serialization of
      -- Timestamp which is the natural ordering due to big-endian
      -- serialization. This allows us to also use the Trie to find the release
      -- with minimal timestamp.
      nrsMap :: !(Trie.TrieN BufferedFix Timestamp AccountSet)
    }
    deriving (Show)

instance MonadBlobStore m => BlobStorable m NewReleaseSchedule where
    storeUpdate NewReleaseSchedule{..} = do
        (pmap, newMap) <- storeUpdate nrsMap
        let !p = do
                put nrsFirstTimestamp
                pmap
        let !rs = NewReleaseSchedule{nrsMap = newMap, ..}
        return (p, rs)
    load = do
        nrsFirstTimestamp <- get
        mmap <- load
        return $! do
            nrsMap <- mmap
            return $! NewReleaseSchedule{..}

instance (MonadBlobStore m) => Cacheable m NewReleaseSchedule where
    cache rs = do
        newMap <- cache (nrsMap rs)
        return $! rs{nrsMap = newMap}

instance (MonadBlobStore m) => ReleaseScheduleOperations m NewReleaseSchedule where
    type AccountRef NewReleaseSchedule = AccountIndex

    addAccountRelease ts ai rs = do
        (_, nrsMap) <- Trie.adjust addAcc ts (nrsMap rs)
        return $!
            NewReleaseSchedule
                { nrsFirstTimestamp = min ts (nrsFirstTimestamp rs),
                  ..
                }
      where
        addAcc Nothing = return ((), Trie.Insert (AccountSet (Set.singleton ai)))
        addAcc (Just (AccountSet accs)) = return $!! ((), Trie.Insert (AccountSet (Set.insert ai accs)))

    updateAccountRelease oldts newts ai rs = do
        (_, rsRem) <- Trie.adjust remAcc oldts (nrsMap rs)
        (_, nrsMap) <- Trie.adjust addAcc newts rsRem
        return $!
            NewReleaseSchedule
                { nrsFirstTimestamp = min newts (nrsFirstTimestamp rs),
                  ..
                }
      where
        remAcc Nothing = error "updateAccountRelease: no entry at expected release time"
        remAcc (Just (AccountSet accs)) =
            return $!
                let accs' = Set.delete ai accs
                in  if Set.null accs' then ((), Trie.Remove) else ((), Trie.Insert (AccountSet accs'))
        addAcc Nothing = return ((), Trie.Insert (AccountSet (Set.singleton ai)))
        addAcc (Just (AccountSet accs)) = return $!! ((), Trie.Insert (AccountSet (Set.insert ai accs)))

    processReleasesUntil ts rs = do
        if ts < nrsFirstTimestamp rs
            then return ([], rs)
            else go [] (nrsMap rs)
      where
        go accum m =
            Trie.findMin m >>= \case
                Nothing -> return $!! (accum, NewReleaseSchedule (Timestamp maxBound) m)
                Just (minTS, accs)
                    | ts < minTS -> return $!! (accum, NewReleaseSchedule minTS m)
                    | otherwise -> do
                        newMap <- Trie.delete minTS m
                        go (accum ++ Set.toList (theAccountSet accs)) newMap

-- |A reference to an account used in the top-level release schedule.
-- For protocol version prior to 'P5', this is 'AccountAddress', and for 'P5' onward this is
-- 'AccountIndex'. This type determines the implementation of the release schedule use for the
-- protocol version.
--
-- Note: the use of 'AccountAddress' (and the 'LegacyReleaseSchedule') is done to maintain database
-- compatibility, rather than as a strict requirement from the protocol itself. The release schedule
-- is essentially an index into the accounts, and so not hashed as part of the state hash.
type family RSAccountRef pv where
    RSAccountRef 'P1 = AccountAddress
    RSAccountRef 'P2 = AccountAddress
    RSAccountRef 'P3 = AccountAddress
    RSAccountRef 'P4 = AccountAddress
    RSAccountRef _ = AccountIndex

-- |A top-level release schedule used for a particular protocol version.
data ReleaseSchedule (pv :: ProtocolVersion) where
    -- |A release schedule for protocol versions 'P1' to 'P4'.
    ReleaseScheduleP0 ::
        (RSAccountRef pv ~ AccountAddress) =>
        !(BufferedRef LegacyReleaseSchedule) ->
        ReleaseSchedule pv
    -- |A release schedule for protocol versions 'P5' onwards.
    ReleaseScheduleP5 ::
        (RSAccountRef pv ~ AccountIndex) =>
        !NewReleaseSchedule ->
        ReleaseSchedule pv

deriving instance IsProtocolVersion pv => Show (ReleaseSchedule pv)

instance (MonadBlobStore m, IsProtocolVersion pv) => BlobStorable m (ReleaseSchedule pv) where
    storeUpdate (ReleaseScheduleP0 rs) = second ReleaseScheduleP0 <$> storeUpdate rs
    storeUpdate (ReleaseScheduleP5 rs) = second ReleaseScheduleP5 <$> storeUpdate rs
    load = case protocolVersion @pv of
        SP1 -> fmap ReleaseScheduleP0 <$> load
        SP2 -> fmap ReleaseScheduleP0 <$> load
        SP3 -> fmap ReleaseScheduleP0 <$> load
        SP4 -> fmap ReleaseScheduleP0 <$> load
        SP5 -> fmap ReleaseScheduleP5 <$> load

instance (MonadBlobStore m) => Cacheable m (ReleaseSchedule pv) where
    cache (ReleaseScheduleP0 rs) = ReleaseScheduleP0 <$> cache rs
    cache (ReleaseScheduleP5 rs) = ReleaseScheduleP5 <$> cache rs

instance (MonadBlobStore m) => ReleaseScheduleOperations m (ReleaseSchedule pv) where
    type AccountRef (ReleaseSchedule pv) = RSAccountRef pv
    addAccountRelease ts addr (ReleaseScheduleP0 rs) =
        ReleaseScheduleP0 <$!> addAccountRelease ts addr rs
    addAccountRelease ts addr (ReleaseScheduleP5 rs) =
        ReleaseScheduleP5 <$!> addAccountRelease ts addr rs
    updateAccountRelease oldts ts addr (ReleaseScheduleP0 rs) =
        ReleaseScheduleP0 <$!> updateAccountRelease oldts ts addr rs
    updateAccountRelease oldts ts addr (ReleaseScheduleP5 rs) =
        ReleaseScheduleP5 <$!> updateAccountRelease oldts ts addr rs
    processReleasesUntil ts (ReleaseScheduleP0 rs) =
        second ReleaseScheduleP0 <$!> processReleasesUntil ts rs
    processReleasesUntil ts (ReleaseScheduleP5 rs) =
        second ReleaseScheduleP5 <$!> processReleasesUntil ts rs

-- |Construct an empty release schedule.
emptyReleaseSchedule :: forall m pv. (IsProtocolVersion pv, MonadBlobStore m) => m (ReleaseSchedule pv)
emptyReleaseSchedule = case protocolVersion @pv of
    SP1 -> rsP0
    SP2 -> rsP0
    SP3 -> rsP0
    SP4 -> rsP0
    SP5 -> rsP1
  where
    rsP0 :: (RSAccountRef pv ~ AccountAddress) => m (ReleaseSchedule pv)
    rsP0 = do
        rsRef <-
            refMake $!
                LegacyReleaseSchedule
                    { lrsFirstTimestamp = Timestamp maxBound,
                      lrsMap = Map.empty,
                      lrsEntryCount = 0
                    }
        return $! ReleaseScheduleP0 rsRef
    rsP1 :: (RSAccountRef pv ~ AccountIndex) => m (ReleaseSchedule pv)
    rsP1 = do
        return $!
            ReleaseScheduleP5
                NewReleaseSchedule
                    { nrsFirstTimestamp = Timestamp maxBound,
                      nrsMap = Trie.empty
                    }

-- |Migration information for a release schedule.
data ReleaseScheduleMigration m oldpv pv where
    -- |Migrate from the legacy release schedule to the legacy release schedule.
    RSMLegacyToLegacy ::
        (RSAccountRef oldpv ~ AccountAddress, RSAccountRef pv ~ AccountAddress) =>
        ReleaseScheduleMigration m oldpv pv
    -- |Migrate from the legacy release schedule to the new release schedule. This requires a
    -- function for resolving account addresses to account indexes.
    RSMLegacyToNew ::
        (RSAccountRef oldpv ~ AccountAddress, RSAccountRef pv ~ AccountIndex) =>
        (AccountAddress -> m AccountIndex) ->
        ReleaseScheduleMigration m oldpv pv
    -- |Migrate from the new release schedule to the new release schedule.
    RSMNewToNew ::
        (RSAccountRef oldpv ~ AccountIndex, RSAccountRef pv ~ AccountIndex) =>
        ReleaseScheduleMigration m oldpv pv

-- |Migration information for migrating from one protocol version to the same protocol version.
trivialReleaseScheduleMigration ::
    forall m pv.
    (IsProtocolVersion pv) =>
    ReleaseScheduleMigration m pv pv
trivialReleaseScheduleMigration = case protocolVersion @pv of
    SP1 -> RSMLegacyToLegacy
    SP2 -> RSMLegacyToLegacy
    SP3 -> RSMLegacyToLegacy
    SP4 -> RSMLegacyToLegacy
    SP5 -> RSMNewToNew

-- |Migrate a release schedule from one protocol version to another, given by a
-- 'ReleaseScheduleMigration'.
migrateReleaseSchedule ::
    (SupportMigration m t) =>
    ReleaseScheduleMigration m oldpv pv ->
    ReleaseSchedule oldpv ->
    t m (ReleaseSchedule pv)
migrateReleaseSchedule RSMLegacyToLegacy (ReleaseScheduleP0 rs) =
    ReleaseScheduleP0 <$!> migrateReference return rs
migrateReleaseSchedule (RSMLegacyToNew resolveAcc) (ReleaseScheduleP0 rsRef) = do
    rs <- lift $ refLoad rsRef
    rsMapList <- lift $ forM (Map.toList (lrsMap rs)) $ \(ts, addrs) -> do
        aiList <- mapM resolveAcc (Set.toList addrs)
        return (ts, AccountSet (Set.fromList aiList))
    -- The next two steps could be done more efficiently by immediately writing
    -- data to disk, but it is unlikely to be very important since there aren't
    -- that many releases expected.
    newMap <- Trie.fromList rsMapList
    (_, newMap') <- storeUpdate newMap
    return $!
        ReleaseScheduleP5
            NewReleaseSchedule
                { nrsFirstTimestamp = lrsFirstTimestamp rs,
                  nrsMap = newMap'
                }
migrateReleaseSchedule RSMNewToNew (ReleaseScheduleP5 rs) = do
    newMap <- Trie.migrateTrieN True return (nrsMap rs)
    return $!
        ReleaseScheduleP5
            NewReleaseSchedule
                { nrsFirstTimestamp = nrsFirstTimestamp rs,
                  nrsMap = newMap
                }

-- |Make a persistent release schedule from an in-memory release schedule.
-- This takes a function for resolving account indexes to account addresses, which is used for
-- protocol versions that use the legacy release schedule.
makePersistentReleaseSchedule ::
    forall m pv.
    (IsProtocolVersion pv, MonadBlobStore m) =>
    -- |Function to resolve an account address from an account index
    (AccountIndex -> AccountAddress) ->
    -- |In-memory release schedule
    Transient.ReleaseSchedule ->
    m (ReleaseSchedule pv)
makePersistentReleaseSchedule getAddr tRS = case protocolVersion @pv of
    SP1 -> rsP0
    SP2 -> rsP0
    SP3 -> rsP0
    SP4 -> rsP0
    SP5 -> rsP1
  where
    rsP0 :: (RSAccountRef pv ~ AccountAddress) => m (ReleaseSchedule pv)
    rsP0 = do
        let tf !entries accIds =
                let newSet = Set.map getAddr accIds
                in  (entries + fromIntegral (Set.size newSet), newSet)
            (lrsEntryCount, lrsMap) = Map.mapAccum tf 0 (Transient.rsMap tRS)
        rsRef <-
            refMake $!
                LegacyReleaseSchedule
                    { lrsFirstTimestamp = Transient.rsFirstTimestamp tRS,
                      ..
                    }
        return $! ReleaseScheduleP0 rsRef
    rsP1 :: (RSAccountRef pv ~ AccountIndex) => m (ReleaseSchedule pv)
    rsP1 = do
        nrsMap <- Trie.fromList (second AccountSet <$> Map.toList (Transient.rsMap tRS))
        return $!
            ReleaseScheduleP5
                NewReleaseSchedule
                    { nrsFirstTimestamp = Transient.rsFirstTimestamp tRS,
                      ..
                    }

-- |(For testing purposes) get the map of the earliest scheduled releases of each account.
releasesMap ::
    (MonadBlobStore m) =>
    (AccountAddress -> m AccountIndex) ->
    ReleaseSchedule pv ->
    m (Map.Map Timestamp (Set.Set AccountIndex))
releasesMap resolveAddr (ReleaseScheduleP0 rsRef) = do
    LegacyReleaseSchedule{..} <- refLoad rsRef
    forM lrsMap $ fmap Set.fromList . mapM resolveAddr . Set.toList
releasesMap _ (ReleaseScheduleP5 rs) = do
    m <- Trie.toMap (nrsMap rs)
    return (theAccountSet <$> m)
