{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Concordium.GlobalState.Persistent.BlockState.AccountReleaseScheduleV1 where

import Control.Monad
import Control.Monad.Trans
import Data.Foldable
import Data.Serialize
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVec
import Data.Word

import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Types
import Concordium.Utils.Serialization

import qualified Concordium.GlobalState.Basic.BlockState.AccountReleaseScheduleV1 as Transient
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.Types.HashableTo
import qualified Concordium.GlobalState.Persistent.BlockState.AccountReleaseSchedule as ARSV0
import qualified Concordium.GlobalState.Basic.BlockState.AccountReleaseSchedule as TARSV0

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
    }

rseSortKey :: ReleaseScheduleEntry -> (Timestamp, Hash.Hash)
rseSortKey ReleaseScheduleEntry{..} = (rseNextTimestamp, rseReleasesHash)

newtype AccountReleaseSchedule = AccountReleaseSchedule
    { -- |The release entries ordered on 'rseSortKey'.
      arsReleases :: Vector ReleaseScheduleEntry
    }

newtype AccountReleaseScheduleHashV1 = AccountReleaseScheduleHashV1
    { theAccountReleaseScheduleHashV1 :: Hash.Hash
    }
    deriving (Eq, Ord, Show, Serialize)

arshV1Empty :: AccountReleaseScheduleHashV1
arshV1Empty = AccountReleaseScheduleHashV1 (Hash.hash "")

arshV1Cons :: Hash.Hash -> AccountReleaseScheduleHashV1 -> AccountReleaseScheduleHashV1
arshV1Cons h arsh = AccountReleaseScheduleHashV1 . Hash.hashLazy . runPutLazy $ put h >> put arsh

instance HashableTo AccountReleaseScheduleHashV1 AccountReleaseSchedule where
    getHash AccountReleaseSchedule{..} = Vector.foldr' (arshV1Cons . rseReleasesHash) arshV1Empty arsReleases

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
    let !newRels = Vector.fromList newRelsList <> staticReleases
    let !nextTS = rseNextTimestamp . fst <$> Vector.uncons newRels
    return (relAmt, nextTS, AccountReleaseSchedule newRels)
  where
    (elapsedReleases, staticReleases) = Vector.break (\r -> ts < rseNextTimestamp r) (arsReleases ars)
    updateEntry (!relAmtAcc, !upds) ReleaseScheduleEntry{..} = do
        rels@Releases{..} <- refLoad rseReleasesRef
        let insert [] e = [e]
            insert (h : t) e
                | rseSortKey e < rseSortKey h = e : h : t
                | otherwise = h : insert t e
            go !n !accum
                | fromIntegral n < Vector.length relReleases =
                    let (relts, amt) = relReleases Vector.! fromIntegral n
                     in if relts <= ts
                            then go (n + 1) (accum + amt)
                            else
                                ( accum + relAmtAcc,
                                  insert upds $!
                                    ReleaseScheduleEntry
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
    TARSV0.AccountReleaseSchedule{..} <- lift $ ARSV0.loadPersistentAccountReleaseSchedule ars
    undefined


makePersistentAccountReleaseSchedule :: MonadBlobStore m => Transient.AccountReleaseSchedule -> m AccountReleaseSchedule
makePersistentAccountReleaseSchedule tars = do
    AccountReleaseSchedule . Vector.fromList <$> mapM mpEntry (Transient.arsReleases tars)
  where
    mpEntry rse@Transient.ReleaseScheduleEntry{..} = do
        let rseNextTimestamp = Transient.rseNextTimestamp rse
        let rseNextReleaseIndex = 0
        rseReleasesRef <-
            refMake $!
                Releases
                    { relTransactionHash = rseTransactionHash,
                      relReleases = Vector.fromList (toList (Transient.relReleases rseReleases))
                    }
        return $! ReleaseScheduleEntry{..}