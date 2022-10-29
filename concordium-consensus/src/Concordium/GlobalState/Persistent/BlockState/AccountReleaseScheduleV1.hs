{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Concordium.GlobalState.Persistent.BlockState.AccountReleaseScheduleV1 where

import Data.List (sortOn)
import Control.Monad
import Control.Monad.Trans
import Data.Foldable
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Serialize
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVec
import Data.Word

import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Types
import Concordium.Types.Accounts.Releases
import Concordium.Utils.Serialization

import qualified Concordium.GlobalState.Basic.BlockState.AccountReleaseScheduleV1 as TARSV1
import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.GlobalState.Persistent.BlockState.AccountReleaseSchedule as ARSV0
import Concordium.Types.HashableTo

data Releases = Releases
    { relTransactionHash :: !TransactionHash,
      relReleases :: !(Vector (Timestamp, Amount))
    }

instance Serialize Releases where
    put Releases{..} = do
        put relTransactionHash
        putLength (Vector.length relReleases)
        Vector.mapM_ put relReleases
    get = do
        relTransactionHash <- get
        len <- getLength
        relReleases <- Vector.replicateM len get
        return Releases{..}

instance MonadBlobStore m => BlobStorable m Releases

hashReleasesFrom :: Word64 -> Releases -> Hash.Hash
hashReleasesFrom dropCount Releases{..} =
    case Vector.unsnoc (Vector.drop (fromIntegral dropCount) relReleases) of
        Nothing -> error "hashReleasesFrom: not enough releases to hash"
        Just (rels, lastRel) -> Vector.foldr' hashStep hashBase rels
          where
            serRel (ts, amt) = put ts >> put amt
            hashBase = Hash.hashLazy $ runPutLazy (serRel lastRel)
            hashStep rel accum = Hash.hashLazy $ runPutLazy $ serRel rel >> put accum

data ReleaseScheduleEntry = ReleaseScheduleEntry
    { -- |Timestamp of the next release.
      rseNextTimestamp :: !Timestamp,
      -- |Hash derived from the releases (given the next release index).
      rseReleasesHash :: !Hash.Hash,
      -- |Reference to the releases.
      rseReleasesRef :: !(LazyBufferedRef Releases),
      -- |Index of the next release.
      rseNextReleaseIndex :: !Word64
    } deriving(Show)

instance (MonadBlobStore m) => BlobStorable m ReleaseScheduleEntry where
    storeUpdate ReleaseScheduleEntry{..} = do
        (pReleases, newReleasesRef) <- storeUpdate rseReleasesRef
        let !p = do
                put rseNextTimestamp
                put rseReleasesHash
                pReleases
                put rseNextReleaseIndex
        return (p, ReleaseScheduleEntry{rseReleasesRef = newReleasesRef, ..})
    load = do
        rseNextTimestamp <- get
        rseReleasesHash <- get
        mReleases <- load
        rseNextReleaseIndex <- get
        return $! do
            rseReleasesRef <- mReleases
            return $! ReleaseScheduleEntry{..}

rseSortKey :: ReleaseScheduleEntry -> (Timestamp, Hash.Hash)
rseSortKey ReleaseScheduleEntry{..} = (rseNextTimestamp, rseReleasesHash)

newtype AccountReleaseSchedule = AccountReleaseSchedule
    { -- |The release entries ordered on 'rseSortKey'.
      arsReleases :: Vector ReleaseScheduleEntry
    }

instance (MonadBlobStore m) => BlobStorable m AccountReleaseSchedule where
    storeUpdate AccountReleaseSchedule{..} = do
        storeReleases <- mapM storeUpdate arsReleases
        let p = do
                putLength (Vector.length arsReleases)
                sequence_ (fst <$> storeReleases)
        return (p, AccountReleaseSchedule $! snd <$!> storeReleases)
    load = do
        len <- getLength
        loads <- Vector.replicateM len load
        return $! AccountReleaseSchedule <$> Vector.sequence loads

instance HashableTo TARSV1.AccountReleaseScheduleHashV1 AccountReleaseSchedule where
    getHash AccountReleaseSchedule{..} = Vector.foldr' (TARSV1.consAccountReleaseScheduleHashV1 . rseReleasesHash) TARSV1.emptyAccountReleaseScheduleHashV1 arsReleases

instance (Monad m) => MHashableTo m TARSV1.AccountReleaseScheduleHashV1 AccountReleaseSchedule

-- | The empty account release schedule.
emptyAccountReleaseSchedule :: AccountReleaseSchedule
emptyAccountReleaseSchedule = AccountReleaseSchedule Vector.empty

-- | Returns 'True' if the account release schedule contains no releases.
isEmptyAccountReleaseSchedule :: AccountReleaseSchedule -> Bool
isEmptyAccountReleaseSchedule = Vector.null . arsReleases

-- |Get the timestamp at which the next scheduled release will occur (if any).
nextReleaseTimestamp :: AccountReleaseSchedule -> Maybe Timestamp
nextReleaseTimestamp AccountReleaseSchedule{..}
    | Vector.length arsReleases == 0 = Nothing
    | otherwise = Just $! rseNextTimestamp (Vector.head arsReleases)

insertEntry :: ReleaseScheduleEntry -> AccountReleaseSchedule -> AccountReleaseSchedule
insertEntry entry AccountReleaseSchedule{..} = AccountReleaseSchedule newReleases
  where
    oldLen = Vector.length arsReleases
    newReleases = Vector.create $ do
        newVec <- MVec.new (oldLen + 1)
        let (prefix, suffix) = Vector.break (\x -> rseSortKey entry <= rseSortKey x) arsReleases
            insertPos = Vector.length prefix
            start = MVec.take insertPos newVec
            end = MVec.drop (insertPos + 1) newVec
        Vector.copy start prefix
        MVec.write newVec insertPos entry
        Vector.copy end suffix
        return newVec

-- | Insert a new schedule in the structure.
--
-- Precondition: The given list of timestamps and amounts MUST NOT be empty.
addReleases :: MonadBlobStore m => ([(Timestamp, Amount)], TransactionHash) -> AccountReleaseSchedule -> m AccountReleaseSchedule
addReleases (rels@((rseNextTimestamp, _) : _), th) ars = do
    let newReleases = Releases th (Vector.fromList rels)
    let rseNextReleaseIndex = 0
    let rseReleasesHash = hashReleasesFrom rseNextReleaseIndex newReleases
    rseReleasesRef <- refMake newReleases
    let newEntry = ReleaseScheduleEntry{..}
    return $! insertEntry newEntry ars
addReleases _ _ = error "addReleases: Empty list of timestamps and amounts."

-- | Returns the amount that was unlocked, the next timestamp for this account
-- (if there is one) and the new account release schedule after removing the
-- amounts whose timestamp was less or equal to the given timestamp.
unlockAmountsUntil :: MonadBlobStore m => Timestamp -> AccountReleaseSchedule -> m (Amount, Maybe Timestamp, AccountReleaseSchedule)
unlockAmountsUntil ts ars = do
    (!relAmt, newRelsList) <- Vector.foldM' updateEntry (0, []) elapsedReleases
    let mergeOrdered [] ys = ys
        mergeOrdered xs [] = xs
        mergeOrdered xxs@(x:xs) yys@(y:ys)
            | rseNextTimestamp x <= rseNextTimestamp y = x:mergeOrdered xs yys
            | otherwise = y:mergeOrdered xxs ys
    let !newRels = Vector.fromList (mergeOrdered newRelsList (Vector.toList staticReleases))
    let !nextTS = rseNextTimestamp . fst <$> Vector.uncons newRels
    return (relAmt, nextTS, AccountReleaseSchedule newRels)
  where
    (elapsedReleases, staticReleases) = Vector.break (\r -> ts < rseNextTimestamp r) (arsReleases ars)
    updateEntry (!relAmtAcc, !upds) ReleaseScheduleEntry{..} = do
        rels@Releases{..} <- refLoad rseReleasesRef
        let insert [] e = [e]
            insert v@(h : t) e
                | rseSortKey e < rseSortKey h = e : v
                | otherwise = h : insert t e
            go !n !accum
                | fromIntegral n < Vector.length relReleases =
                    let (relts, amt) = relReleases Vector.! fromIntegral n
                     in if relts <= ts
                            then go (n + 1) (accum + amt)
                            else
                                ( accum + relAmtAcc,
                                  insert upds
                                    $! ReleaseScheduleEntry
                                        { rseNextReleaseIndex = n,
                                          rseNextTimestamp = relts,
                                          rseReleasesHash = hashReleasesFrom n rels,
                                          ..
                                        }
                                )
                | otherwise = (accum + relAmtAcc, upds)
        return $! go rseNextReleaseIndex 0

-- |Migrate an account release schedule for a protocol update.
migrateAccountReleaseSchedule :: SupportMigration m t => AccountReleaseSchedule -> t m AccountReleaseSchedule
migrateAccountReleaseSchedule AccountReleaseSchedule{..} = AccountReleaseSchedule <$!> mapM migrateEntry arsReleases
  where
    migrateEntry ReleaseScheduleEntry{..} = do
        -- This could be made more efficient by dropping the already-released amounts at migration.
        -- For now, we opt for simplicity instead.
        newReleasesRef <- migrateReference return rseReleasesRef
        return $! ReleaseScheduleEntry{rseReleasesRef = newReleasesRef, ..}

migrateAccountReleaseScheduleFromV0 :: SupportMigration m t => ARSV0.AccountReleaseSchedule -> t m AccountReleaseSchedule
migrateAccountReleaseScheduleFromV0 ars = do
    tarsV0 <- lift $ ARSV0.loadPersistentAccountReleaseSchedule ars
    makePersistentAccountReleaseSchedule $ TARSV1.fromAccountReleaseScheduleV0 tarsV0

-- |Serialize an 'AccountReleaseSchedule' in the serialization format for
-- 'TARSV1.AccountReleaseSchedule'.
serializeAccountReleaseSchedule :: forall m. (MonadBlobStore m) => AccountReleaseSchedule -> m Put
serializeAccountReleaseSchedule AccountReleaseSchedule{..} = do
    foldlM putEntry putLen arsReleases
  where
    putLen = putLength (Vector.length arsReleases)
    putEntry :: Put -> ReleaseScheduleEntry -> m Put
    putEntry put0 ReleaseScheduleEntry{..} = do
        Releases{..} <- refLoad rseReleasesRef
        let start = fromIntegral rseNextReleaseIndex
        return $ do
            put0
            put relTransactionHash
            putLength (Vector.length relReleases - start)
            mapM_ put (Vector.drop start relReleases)

makePersistentAccountReleaseSchedule :: MonadBlobStore m => TARSV1.AccountReleaseSchedule -> m AccountReleaseSchedule
makePersistentAccountReleaseSchedule tars = do
    AccountReleaseSchedule . Vector.fromList <$> mapM mpEntry (TARSV1.arsReleases tars)
  where
    mpEntry rse@TARSV1.ReleaseScheduleEntry{..} = do
        let rseNextTimestamp = TARSV1.rseNextTimestamp rse
        let rseNextReleaseIndex = 0
        rseReleasesRef <-
            refMake
                $! Releases
                    { relTransactionHash = rseTransactionHash,
                      relReleases = Vector.fromList (toList (TARSV1.relReleases rseReleases))
                    }
        return $! ReleaseScheduleEntry{..}

-- |Convert an 'AccountReleaseSchedule' to a  transient 'TARSV1.AccountReleaseSchedule', given
-- the total locked amount on the account.
getAccountReleaseSchedule ::
    MonadBlobStore m =>
    -- |Total locked amount
    Amount ->
    AccountReleaseSchedule ->
    m TARSV1.AccountReleaseSchedule
getAccountReleaseSchedule arsTotalLockedAmount AccountReleaseSchedule{..} = do
    releases <- foldrM processEntry [] arsReleases
    return $! TARSV1.AccountReleaseSchedule{arsReleases = sortOn TARSV1.rseSortKey releases, ..}
  where
    processEntry ReleaseScheduleEntry{..} entries = do
        Releases{..} <- refLoad rseReleasesRef
        let releases = NE.fromList $ Vector.toList $ Vector.drop (fromIntegral rseNextReleaseIndex) relReleases
        let entry =
                TARSV1.ReleaseScheduleEntry
                    { rseReleases = TARSV1.Releases releases,
                      rseReleasesHash = rseReleasesHash,
                      rseTransactionHash = relTransactionHash
                    }
        return $ entry : entries

-- |Get the 'AccountReleaseSummary' describing the releases in the 'AccountReleaseSchedule'.
toAccountReleaseSummary :: MonadBlobStore m => AccountReleaseSchedule -> m AccountReleaseSummary
toAccountReleaseSummary AccountReleaseSchedule{..} = do
    (releaseMap, releaseTotal) <- foldlM processEntry (Map.empty, 0) arsReleases
    let releaseSchedule = makeSR <$> Map.toList releaseMap
    return $! AccountReleaseSummary{..}
  where
    agg (amt1, ths1) (amt2, ths2) = (amt, ths)
      where
        !amt = amt1 + amt2
        ths = ths1 ++ ths2
    processRelease th (!m, !acc) (ts, amt) = (Map.insertWith agg ts (amt, [th]) m, acc + amt)
    processEntry acc ReleaseScheduleEntry{..} = do
        Releases{..} <- refLoad rseReleasesRef
        let rels = Vector.drop (fromIntegral rseNextReleaseIndex) relReleases
        return $! foldl' (processRelease relTransactionHash) acc rels
    makeSR (releaseTimestamp, (releaseAmount, releaseTransactions)) = ScheduledRelease{..}
