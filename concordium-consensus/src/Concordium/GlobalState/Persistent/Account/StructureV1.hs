{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |This module implements accounts for account versions 'AccountV2' (protocol 'P5').
-- It should not be necessary to use this module directly, but instead through the interface
-- provided by 'Concordium.GlobalState.Persistent.Account'.
module Concordium.GlobalState.Persistent.Account.StructureV1 where

import Control.Monad
import Data.Bits
import Data.Foldable
import Data.Serialize
import Data.Word
import Lens.Micro.Platform

import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.ID.Types hiding (values)
import Concordium.Types
import Concordium.Types.Accounts
import Concordium.Types.Execution
import Concordium.Types.HashableTo
import Concordium.Types.Parameters
import Concordium.Utils

import Concordium.Genesis.Data
import Concordium.GlobalState.Account hiding (addIncomingEncryptedAmount, addToSelfEncryptedAmount, replaceUpTo)
import Concordium.GlobalState.BakerInfo
import qualified Concordium.GlobalState.Basic.BlockState.Account as Transient
import qualified Concordium.GlobalState.Basic.BlockState.AccountReleaseSchedule as TARS
import qualified Concordium.GlobalState.Basic.BlockState.AccountReleaseScheduleV1 as TARSV1
import Concordium.GlobalState.BlockState (AccountAllowance (..))
import Concordium.GlobalState.Persistent.Account.EncryptedAmount
import qualified Concordium.GlobalState.Persistent.Account.StructureV0 as V0
import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.GlobalState.Persistent.BlockState.AccountReleaseSchedule as ARSV0
import Concordium.GlobalState.Persistent.BlockState.AccountReleaseScheduleV1
import Concordium.ID.Parameters
import Concordium.Types.Accounts.Releases
import Concordium.Utils.Serialization
import Concordium.Utils.Serialization.Put
import Control.Monad.Trans
import qualified Data.Map.Strict as Map

-- * Terminology

-- $Terminology
--
-- The terms "persistent", "persisting" and "enduring" have separate meanings in our nomenclature:
--
-- * "Persistent" is used to make explicit that the datastructure is persisted in the blob store.
--
-- * "Persisting" is used specifically to refer to 'PersistingAccountData'.
--
-- * "Enduring" is used to refer to that data encapsulated by 'PersistentAccountEnduringData',
--   which consists of all account data except nonce, current balance, and staked balance.

-- * 'PersistentBakerInfoEx'

-- |A reference to a 'BakerInfoEx'. These references are shared between baker accounts
-- and the current/next epoch bakers. We use a 'LazyBufferedRef' so that the reference does not
-- need to be loaded whenever the account is loaded, but once it is loaded, copies of the reference
-- (e.g. in the account cache) will also be loaded.
type PersistentBakerInfoEx av = LazyBufferedRef (BakerInfoEx av)

-- ** Query

-- |Load 'BakerInfo' from a 'PersistentBakerInfoEx'.
loadBakerInfo :: (MonadBlobStore m, IsAccountVersion av) => PersistentBakerInfoEx av -> m BakerInfo
loadBakerInfo = fmap _bieBakerInfo . refLoad

-- |Load a 'BakerInfoEx' from a 'PersistentBakerInfoEx'.
loadPersistentBakerInfoEx ::
    (MonadBlobStore m, IsAccountVersion av) =>
    PersistentBakerInfoEx av ->
    m (BakerInfoEx av)
loadPersistentBakerInfoEx = refLoad

-- |Load the 'BakerId' from a 'PersistentBakerInfoEx'.
loadBakerId :: (MonadBlobStore m, IsAccountVersion av) => PersistentBakerInfoEx av -> m BakerId
loadBakerId = fmap (view bakerIdentity) . refLoad

-- ** Construction

-- |Construct a 'PersistentBakerInfoEx' from a 'BakerInfoEx'.
makePersistentBakerInfoEx :: (MonadBlobStore m, IsAccountVersion av) => BakerInfoEx av -> m (PersistentBakerInfoEx av)
makePersistentBakerInfoEx = refMake

-- ** Migration

-- |See documentation of @migratePersistentBlockState@.
migratePersistentBakerInfoEx ::
    ( AccountVersionFor oldpv ~ 'AccountV2,
      SupportMigration m t
    ) =>
    StateMigrationParameters oldpv pv ->
    PersistentBakerInfoEx (AccountVersionFor oldpv) ->
    t m (PersistentBakerInfoEx (AccountVersionFor pv))
migratePersistentBakerInfoEx StateMigrationParametersTrivial = migrateReference return

-- |Migrate a 'V0.PersistentBakerInfoEx' to a 'PersistentBakerInfoEx'.
-- See documentation of @migratePersistentBlockState@.
migratePersistentBakerInfoExFromV0 ::
    ( AccountVersionFor oldpv ~ 'AccountV1,
      AccountVersionFor pv ~ 'AccountV2,
      SupportMigration m t
    ) =>
    StateMigrationParameters oldpv pv ->
    V0.PersistentBakerInfoEx (AccountVersionFor oldpv) ->
    t m (PersistentBakerInfoEx (AccountVersionFor pv))
migratePersistentBakerInfoExFromV0 StateMigrationParametersP4ToP5{} V0.PersistentBakerInfoEx{..} = do
    bkrInfoEx <- lift $ do
        bkrInfo <- refLoad bakerInfoRef
        bkrPoolInfo <- refLoad (V0._theExtraBakerInfo bakerInfoExtra)
        return $! BakerInfoExV1 bkrInfo bkrPoolInfo
    (ref, _) <- refFlush =<< refMake bkrInfoEx
    return $! ref

-- * Enduring account stake data

-- |This is the information about the stake associated with an account, excluding the staked
-- amount. The staked amount is excluded as it expected to change commonly for accounts that
-- restake their earnings.
--
-- Note, only the baker info is stored under a reference. The reference is a 'LazyBufferedRef'
-- rather than an 'EagerBufferedRef', as it will often be unnecessary to load the baker info.
data PersistentAccountStakeEnduring av where
    PersistentAccountStakeEnduringNone :: PersistentAccountStakeEnduring av
    PersistentAccountStakeEnduringBaker ::
        { paseBakerRestakeEarnings :: !Bool,
          paseBakerInfo :: !(LazyBufferedRef (BakerInfoEx av)),
          paseBakerPendingChange :: !(StakePendingChange' Timestamp)
        } ->
        PersistentAccountStakeEnduring av
    PersistentAccountStakeEnduringDelegator ::
        { paseDelegatorId :: !DelegatorId,
          paseDelegatorRestakeEarnings :: !Bool,
          paseDelegatorTarget :: !DelegationTarget,
          paseDelegatorPendingChange :: !(StakePendingChange' Timestamp)
        } ->
        PersistentAccountStakeEnduring av

-- |Convert a 'PersistentAccountStakeEnduring' to an 'AccountStake' given the amount of the stake.
-- This is used to implement 'getStake', and is also used in computing the stake hash.
persistentToAccountStake ::
    (MonadBlobStore m, IsAccountVersion av, AVSupportsDelegation av) =>
    PersistentAccountStakeEnduring av ->
    Amount ->
    m (AccountStake av)
persistentToAccountStake PersistentAccountStakeEnduringNone _ = return AccountStakeNone
persistentToAccountStake PersistentAccountStakeEnduringBaker{..} _stakedAmount = do
    _accountBakerInfo <- refLoad paseBakerInfo
    return $!
        AccountStakeBaker
            AccountBaker
                { _stakeEarnings = paseBakerRestakeEarnings,
                  _bakerPendingChange = PendingChangeEffectiveV1 <$> paseBakerPendingChange,
                  ..
                }
persistentToAccountStake PersistentAccountStakeEnduringDelegator{..} _delegationStakedAmount = do
    return $!
        AccountStakeDelegate
            AccountDelegationV1
                { _delegationIdentity = paseDelegatorId,
                  _delegationStakeEarnings = paseDelegatorRestakeEarnings,
                  _delegationTarget = paseDelegatorTarget,
                  _delegationPendingChange = PendingChangeEffectiveV1 <$> paseDelegatorPendingChange,
                  ..
                }

-- |Migrate a 'PersistentAccountStakeEnduring' from one blob store to another.
migratePersistentAccountStakeEnduring ::
    (SupportMigration m t, IsAccountVersion av) =>
    PersistentAccountStakeEnduring av ->
    t m (PersistentAccountStakeEnduring av)
migratePersistentAccountStakeEnduring PersistentAccountStakeEnduringNone =
    return PersistentAccountStakeEnduringNone
migratePersistentAccountStakeEnduring PersistentAccountStakeEnduringBaker{..} = do
    newBakerInfo <- migrateReference return paseBakerInfo
    return $!
        PersistentAccountStakeEnduringBaker
            { paseBakerInfo = newBakerInfo,
              ..
            }
migratePersistentAccountStakeEnduring PersistentAccountStakeEnduringDelegator{..} =
    return $! PersistentAccountStakeEnduringDelegator{..}

-- |This relies on the fact that the 'AccountV2' hashing of 'AccountStake' is independent of the
-- staked amount.
instance (MonadBlobStore m) => MHashableTo m (AccountStakeHash 'AccountV2) (PersistentAccountStakeEnduring 'AccountV2) where
    getHashM stake = getHash <$> persistentToAccountStake stake 0

-- * Enduring account data

-- |Enduring data associated with an account. This is data that does not change very often.
-- The 'AccountMerkleHash' is computed and stored for this data so that we can avoid deserializing
-- elements of it unnecessarily.
--
-- The persisting account data is stored under an 'EagerBufferedRef' as this includes the account
-- keys, which are required for any transaction originating from the account, and so are loaded
-- eagerly.
--
-- The encrypted amount is 'Nullable' so that accounts with no encrypted balance can be represented
-- succinctly.  The encrypted balance is stored as a 'LazyBufferedRef', which is not automatically
-- cached with the account, since loading the encrypted balance is relatively expensive and likely
-- not necessary for many operations.
--
-- The release schedule is likewise 'Nullable' and stored under a 'LazyBufferedRef' for similar reasons.
-- However, as well as the 'LazyBufferedRef', we store the locked balance (if it is non-trivial).
-- This is because we typically require ready access to the locked balance so that we can compute
-- the available balance.
--
-- The stake is not stored under a reference (excepting the baker info, as per the definition of
-- 'PersistentAccountStakeEnduring'). This is since the information is relatively succinct.
data PersistentAccountEnduringData (av :: AccountVersion) = PersistentAccountEnduringData
    { -- |The Merkle hash computed from the other fields.
      paedHash :: !(AccountMerkleHash av),
      -- |A reference to the persisting account data.
      paedPersistingData :: !(EagerBufferedRef PersistingAccountData),
      -- |The encrypted amount. Invariant: if this is present, it will not satisfy
      -- 'isInitialPersistentAccountEncryptedAmount'.
      paedEncryptedAmount :: !(Nullable (LazyBufferedRef PersistentAccountEncryptedAmount)),
      -- |The release schedule and total locked amount. Invariant: if this is present,
      -- it does not satisfy 'isEmptyAccountReleaseSchedule', and the amount will be the total of them.
      paedReleaseSchedule :: !(Nullable (LazyBufferedRef AccountReleaseSchedule, Amount)),
      -- |The staking details associated with the account.
      paedStake :: !(PersistentAccountStakeEnduring av)
    }

-- |Get the locked amount from a 'PersistingAccountEnduringData'.
paedLockedAmount :: PersistentAccountEnduringData av -> Amount
paedLockedAmount PersistentAccountEnduringData{..} = case paedReleaseSchedule of
    Some (_, amt) -> amt
    Null -> 0

instance HashableTo (AccountMerkleHash av) (PersistentAccountEnduringData av) where
    getHash = paedHash

-- |Construct a 'PersistentAccountEnduringData' from the components by computing the hash.
--
-- Precondition: if the 'PersistentAccountEncryptedAmount' is present then it must not satisfy
-- 'isInitialPersistentAccountEncryptedAmount'.
--
-- Precondition: if the 'AccountReleaseSchedule' is present, then it must have some releases
-- and the total amount of the releases must be the provided amount.
makeAccountEnduringData ::
    ( MonadBlobStore m
    ) =>
    EagerBufferedRef PersistingAccountData ->
    Nullable (LazyBufferedRef PersistentAccountEncryptedAmount) ->
    Nullable (LazyBufferedRef AccountReleaseSchedule, Amount) ->
    PersistentAccountStakeEnduring 'AccountV2 ->
    m (PersistentAccountEnduringData 'AccountV2)
makeAccountEnduringData paedPersistingData paedEncryptedAmount paedReleaseSchedule paedStake = do
    amhi2PersistingAccountDataHash <- getHashM paedPersistingData
    (amhi2AccountStakeHash :: AccountStakeHash 'AccountV2) <- getHashM paedStake
    amhi2EncryptedAmountHash <- case paedEncryptedAmount of
        Null -> return initialAccountEncryptedAmountHash
        Some e -> getHash <$> (loadPersistentAccountEncryptedAmount =<< refLoad e)
    amhi2AccountReleaseScheduleHash <- case paedReleaseSchedule of
        Null -> return TARSV1.emptyAccountReleaseScheduleHashV1
        Some (rs, _) -> getHashM rs
    let hashInputs :: AccountMerkleHashInputs 'AccountV2
        hashInputs = AccountMerkleHashInputsV2{..}
        !paedHash = getHash hashInputs
    return $! PersistentAccountEnduringData{..}

-- |[For internal use in this module.] Recompute the Merkle hash of the enduring account data.
rehashAccountEnduringData :: (MonadBlobStore m) => PersistentAccountEnduringData 'AccountV2 -> m (PersistentAccountEnduringData 'AccountV2)
rehashAccountEnduringData ed = do
    amhi2PersistingAccountDataHash <- getHashM (paedPersistingData ed)
    (amhi2AccountStakeHash :: AccountStakeHash 'AccountV2) <- getHashM (paedStake ed)
    amhi2EncryptedAmountHash <- case paedEncryptedAmount ed of
        Null -> return initialAccountEncryptedAmountHash
        Some e -> getHash <$> (loadPersistentAccountEncryptedAmount =<< refLoad e)
    amhi2AccountReleaseScheduleHash <- case paedReleaseSchedule ed of
        Null -> return TARSV1.emptyAccountReleaseScheduleHashV1
        Some (rs, _) -> getHashM rs
    let hashInputs :: AccountMerkleHashInputs 'AccountV2
        hashInputs = AccountMerkleHashInputsV2{..}
    return $! ed{paedHash = getHash hashInputs}

enduringDataFlags :: PersistentAccountEnduringData av -> EnduringDataFlags
enduringDataFlags PersistentAccountEnduringData{..} =
    EnduringDataFlags
        { edHasEncryptedAmount = isNotNull paedEncryptedAmount,
          edHasReleaseSchedule = isNotNull paedReleaseSchedule,
          edStakeFlags = stakeFlags paedStake
        }

-- * Enduring account data storage helper definitions

-- |The nature of a pending stake change, abstracting the details.
-- This is stored as the low-order 2 bits of a 'Word8'.
-- The encoding is as follows:
--
-- - Bits 1 and 0 are both unset if there is no pending change.
--
-- - Bit 1 is unset and bit 0 is set if the stake is being reduced.
--
-- - Bit 1 is set and bit 0 is unset if the stake is being removed.
data PendingChangeFlags
    = -- |No change is pending
      PendingChangeNone
    | -- |A stake reduction is pending
      PendingChangeReduce
    | -- |Removal of stake is pending
      PendingChangeRemove
    deriving (Eq, Ord, Show)

-- |Get the 'PendingChangeFlags' for a 'StakePendingChange''.
stakePendingChangeFlags :: StakePendingChange' et -> PendingChangeFlags
stakePendingChangeFlags NoChange = PendingChangeNone
stakePendingChangeFlags ReduceStake{} = PendingChangeReduce
stakePendingChangeFlags RemoveStake{} = PendingChangeRemove

-- |Store a 'PendingChangeFlags' as the low-order 2 bits of a 'Word8'.
pendingChangeFlagsToBits :: PendingChangeFlags -> Word8
pendingChangeFlagsToBits PendingChangeNone = 0b00
pendingChangeFlagsToBits PendingChangeReduce = 0b01
pendingChangeFlagsToBits PendingChangeRemove = 0b10

-- |Load a 'PendingChangeFlags' from the low-order 2 bits of a 'Word8'.
-- All other bits must be 0.
pendingChangeFlagsFromBits :: Word8 -> Either String PendingChangeFlags
pendingChangeFlagsFromBits 0b00 = return PendingChangeNone
pendingChangeFlagsFromBits 0b01 = return PendingChangeReduce
pendingChangeFlagsFromBits 0b10 = return PendingChangeRemove
pendingChangeFlagsFromBits _ = Left "Invalid pending change type"

-- |Flags that represent the nature of the stake on an account.
-- These are stored as the low-order 6 bits of a 'Word8'.
-- The encoding is as follows:
--
-- - Bits 5 and 4 indicate the staking status of the account:
--
--   - If bits 5 and 4 are unset, there is no staking. The remaining bit are also unset.
--
--   - If bit 5 is unset and bit 4 is set, the account is a baker. In this case
--
--     - Bit 3 is unset.
--
--     - Bit 2 is set if earnings are restaked.
--
--     - Bits 1 and 0 indicate the pending change as described in 'PendingChangeFlags'.
--
--    - If bit 5 is set and bit 4 is unset, the account is a delegator. In this case
--
--      - Bit 3 is set if the delegation is passive.
--
--     - Bit 2 is set if earnings are restaked.
--
--     - Bits 1 and 0 indicate the pending change as described in 'PendingChangeFlags'.
data StakeFlags
    = -- |The account is not staking
      StakeFlagsNone
    | -- |The account is a baker
      StakeFlagsBaker
        { -- |Whether earnings are restaked
          sfRestake :: !Bool,
          -- |The pending stake change, if any
          sfChangeType :: !PendingChangeFlags
        }
    | -- |The account is a delegator
      StakeFlagsDelegator
        { -- |Whether delegation is passive
          sfPassive :: !Bool,
          -- |Whether earnings are restaked
          sfRestake :: !Bool,
          -- |The pending stake change, if any
          sfChangeType :: !PendingChangeFlags
        }
    deriving (Eq, Ord, Show)

-- |Get the 'StakeFlags' from a 'PersistentAccountStakeEnduring'.
stakeFlags :: PersistentAccountStakeEnduring av -> StakeFlags
stakeFlags PersistentAccountStakeEnduringNone = StakeFlagsNone
stakeFlags PersistentAccountStakeEnduringBaker{..} =
    StakeFlagsBaker
        { sfRestake = paseBakerRestakeEarnings,
          sfChangeType = stakePendingChangeFlags paseBakerPendingChange
        }
stakeFlags PersistentAccountStakeEnduringDelegator{..} =
    StakeFlagsDelegator
        { sfPassive = DelegatePassive == paseDelegatorTarget,
          sfRestake = paseDelegatorRestakeEarnings,
          sfChangeType = stakePendingChangeFlags paseDelegatorPendingChange
        }

-- |Store a 'StakeFlags' as the low-order 6 bits of a 'Word8'.
stakeFlagsToBits :: StakeFlags -> Word8
stakeFlagsToBits StakeFlagsNone =
    0b00_0000
stakeFlagsToBits StakeFlagsBaker{..} =
    0b01_0000
        .|. (if sfRestake then 0b00_0100 else 0b00_0000)
        .|. pendingChangeFlagsToBits sfChangeType
stakeFlagsToBits StakeFlagsDelegator{..} =
    0b10_0000
        .|. (if sfPassive then 0b00_1000 else 0b00_0000)
        .|. (if sfRestake then 0b00_0100 else 0b00_0000)
        .|. pendingChangeFlagsToBits sfChangeType

-- |Load a 'StakeFlags' from the low-order 6 bits of a 'Word8'.
-- All other bits must be 0.
stakeFlagsFromBits :: Word8 -> Either String StakeFlags
stakeFlagsFromBits 0b00_0000 = return StakeFlagsNone
stakeFlagsFromBits bs = case bs .&. 0b11_0000 of
    0b01_0000 -> do
        when sfPassive $ Left "Passive bit cannot be set for baker"
        sfChangeType <- pendingChangeFlagsFromBits (bs .&. 0b11)
        return StakeFlagsBaker{..}
    0b10_0000 -> do
        sfChangeType <- pendingChangeFlagsFromBits (bs .&. 0b11)
        return StakeFlagsDelegator{..}
    _ -> Left "Invalid staking type"
  where
    sfRestake = testBit bs 2
    sfPassive = testBit bs 3

-- |Flags that represent the nature of enduring account data.
-- These are stored as a 'Word8', in the following format:
--
-- - Bit 7 is set if the account has a (non-initial) encrypted amount.
--
-- - Bit 6 is set if the account has a (non-empty) release schedule.
--
-- - The remaining bits indicate the staking status of the account, in accordance with 'StakeFlags'.
data EnduringDataFlags = EnduringDataFlags
    { -- |Whether the enduring data includes a (non-initial) encrypted amount.
      edHasEncryptedAmount :: !Bool,
      -- |Whether the enduring data includes a (non-empty) release schedule.
      edHasReleaseSchedule :: !Bool,
      -- |Flags describing the stake (if any).
      edStakeFlags :: !StakeFlags
    }
    deriving (Eq, Ord, Show)

-- |Encode an 'EnduringDataFlags' as a 'Word8'.
enduringDataFlagsToBits :: EnduringDataFlags -> Word8
enduringDataFlagsToBits EnduringDataFlags{..} =
    (if edHasEncryptedAmount then 0b1000_0000 else 0b0000_0000)
        .|. (if edHasReleaseSchedule then 0b0100_0000 else 0b0000_0000)
        .|. stakeFlagsToBits edStakeFlags

-- |Decode an 'EnduringDataFlags' from a 'Word8'.
enduringDataFlagsFromBits :: Word8 -> Either String EnduringDataFlags
enduringDataFlagsFromBits bs = do
    let edHasEncryptedAmount = testBit bs 7
    let edHasReleaseSchedule = testBit bs 6
    edStakeFlags <- stakeFlagsFromBits (bs .&. 0b0011_1111)
    return EnduringDataFlags{..}

instance Serialize EnduringDataFlags where
    put = putWord8 . enduringDataFlagsToBits
    get = label "EnduringDataFlags" $ do
        bs <- getWord8
        case enduringDataFlagsFromBits bs of
            Left e -> fail e
            Right r -> return r

-- |'PersistentAccountEnduringData' is stored in the following format:
--
-- 1. The 'AccountMerkleHash'.
-- 2. The 'EnduringDataFlags' (1 byte), which indicate which other fields are present.
-- 3. A reference to the 'PersistingAccountData'.
-- 4. If 'edHasEncryptedAmount' is set, a reference to the 'PersistentAccountEncryptedAmount'.
-- 5. If 'edHasReleaseSchedule' is set, a reference to the 'AccountReleaseSchedule'.
-- 6. Depending on 'edStakeFlags', one of:
--   - If it is 'StakeFlagsNone' then nothing else (there is no staking on the account).
--   - If it is 'StakeFlagsBaker' then:
--     1. A reference to the 'BakerInfoEx'.
--     2. Depending on 'sfChangeType', one of:
--        - If it is 'PendingChangeNone' then nothing else.
--        - If it is 'PendingChangeReduce' then the target amount and effective time.
--        - If it is 'PendingChangeRemove' then the effective time.
--   - If it is 'StakeFlagsDelegator' then:
--     1. The delegator Id
--     2. If 'sfPassive' is not set, then the target baker id.
--     3. Depending on 'sfChangeType', one of:
--        - If it is 'PendingChangeNone' then nothing else.
--        - If it is 'PendingChangeReduce' then the target amount and effective time.
--        - If it is 'PendingChangeRemove' then the effective time.
instance (MonadBlobStore m, IsAccountVersion av) => BlobStorable m (PersistentAccountEnduringData av) where
    storeUpdate paed@PersistentAccountEnduringData{..} = do
        (ppd, newPersistingData) <- storeUpdate paedPersistingData
        (pea, newEncryptedAmount) <- storeUpdate paedEncryptedAmount
        (prs, newReleaseSchedule) <- storeUpdate paedReleaseSchedule
        (ps, newStake) <- suStake paedStake
        let p = do
                put paedHash
                put flags
                ppd
                when (edHasEncryptedAmount flags) pea
                when (edHasReleaseSchedule flags) prs
                ps
            newpaed =
                paed
                    { paedPersistingData = newPersistingData,
                      paedEncryptedAmount = newEncryptedAmount,
                      paedReleaseSchedule = newReleaseSchedule,
                      paedStake = newStake
                    }
        return $!! (p, newpaed)
      where
        flags = enduringDataFlags paed
        -- The flags encode the type of the pending change, so serPC only serializes the data.
        serPC NoChange = return ()
        serPC (ReduceStake amt et) = put amt >> put et
        serPC (RemoveStake et) = put et
        suStake s@PersistentAccountStakeEnduringNone = return (return (), s)
        suStake s@PersistentAccountStakeEnduringBaker{..} = do
            (pinfo, newInfo) <- storeUpdate paseBakerInfo
            let ppc = serPC paseBakerPendingChange
            return $!! (pinfo >> ppc, s{paseBakerInfo = newInfo})
        suStake s@PersistentAccountStakeEnduringDelegator{..} = do
            let ptarget = case paseDelegatorTarget of
                    DelegatePassive -> return ()
                    DelegateToBaker b -> put b
            let ppc = serPC paseDelegatorPendingChange
            return $!! (put paseDelegatorId >> ptarget >> ppc, s)
    load = do
        paedHash <- get
        EnduringDataFlags{..} <- get
        mPersistingData <- load
        mEncryptedAmount <-
            if edHasEncryptedAmount
                then load
                else return (return Null)
        mReleaseSchedule <- if edHasReleaseSchedule then load else return (return Null)
        let getPC PendingChangeNone = return NoChange
            getPC PendingChangeReduce = ReduceStake <$> get <*> get
            getPC PendingChangeRemove = RemoveStake <$> get
        mStake <- case edStakeFlags of
            StakeFlagsNone -> return (return PersistentAccountStakeEnduringNone)
            StakeFlagsBaker{..} -> do
                let paseBakerRestakeEarnings = sfRestake
                mInfo <- load
                paseBakerPendingChange <- getPC sfChangeType
                return $! do
                    paseBakerInfo <- mInfo
                    return PersistentAccountStakeEnduringBaker{..}
            StakeFlagsDelegator{..} -> do
                paseDelegatorId <- get
                let paseDelegatorRestakeEarnings = sfRestake
                paseDelegatorTarget <- if sfPassive then return DelegatePassive else DelegateToBaker <$> get
                paseDelegatorPendingChange <- getPC sfChangeType
                return . return $! PersistentAccountStakeEnduringDelegator{..}
        return $! do
            paedPersistingData <- mPersistingData
            paedEncryptedAmount <- mEncryptedAmount
            paedReleaseSchedule <- mReleaseSchedule
            paedStake <- mStake
            return PersistentAccountEnduringData{..}

-- * Persistent account

-- |A persistent account.
-- The most commonly modified fields of an account (the next nonce, balance and staked balance)
-- are directly available.  The rest of the fields are stored as part of the enduring data,
-- under an 'EagerBufferedRef'.  This limits the amount that needs to be rewritten for the
-- most common updates.
data PersistentAccount av = PersistentAccount
    { -- |The next nonce for transactions on the account.
      accountNonce :: !Nonce,
      -- |The total balance of the account.
      accountAmount :: !Amount,
      -- |The staked balance of the account.
      -- INVARIANT: This is 0 if the account is not a baker or delegator.
      accountStakedAmount :: !Amount,
      -- |The enduring account data.
      accountEnduringData :: !(EagerBufferedRef (PersistentAccountEnduringData av))
    }

instance HashableTo (AccountHash 'AccountV2) (PersistentAccount 'AccountV2) where
    getHash PersistentAccount{..} =
        makeAccountHash $
            AHIV2 $
                AccountHashInputsV2
                    { ahi2NextNonce = accountNonce,
                      ahi2AccountBalance = accountAmount,
                      ahi2StakedBalance = accountStakedAmount,
                      ahi2MerkleHash = getHash accountEnduringData
                    }

instance Monad m => MHashableTo m (AccountHash 'AccountV2) (PersistentAccount 'AccountV2)

instance HashableTo Hash.Hash (PersistentAccount 'AccountV2) where
    getHash = theAccountHash @'AccountV2 . getHash

instance Monad m => MHashableTo m Hash.Hash (PersistentAccount 'AccountV2)

instance (MonadBlobStore m, IsAccountVersion av) => BlobStorable m (PersistentAccount av) where
    storeUpdate acc@PersistentAccount{..} = do
        (pEnduringData, newEnduringData) <- storeUpdate accountEnduringData
        let p = do
                put accountNonce
                put accountAmount
                put accountStakedAmount
                pEnduringData
        return $!! (p, acc{accountEnduringData = newEnduringData})
    load = do
        accountNonce <- get
        accountAmount <- get
        accountStakedAmount <- get
        mEnduringData <- load
        return $ do
            accountEnduringData <- mEnduringData
            return $! PersistentAccount{..}

-- |Get the enduring data for an account.
enduringData :: PersistentAccount av -> PersistentAccountEnduringData av
enduringData = eagerBufferedDeref . accountEnduringData

-- |Get the persisting data for an account.
persistingData :: PersistentAccount av -> PersistingAccountData
persistingData = eagerBufferedDeref . paedPersistingData . enduringData

-- ** Queries

-- |Get the canonical address of the account.
getCanonicalAddress :: (Monad m) => PersistentAccount av -> m AccountAddress
getCanonicalAddress acc = do
    let pd = persistingData acc
    return $! pd ^. accountAddress

-- |Get the current public account balance.
getAmount :: (Monad m) => PersistentAccount av -> m Amount
getAmount = pure . accountAmount

-- |Gets the amount of a baker's stake, or 'Nothing' if the account is not a baker.
getBakerStakeAmount :: (Monad m) => PersistentAccount av -> m (Maybe Amount)
getBakerStakeAmount acc = do
    let ed = enduringData acc
    return $! case paedStake ed of
        PersistentAccountStakeEnduringBaker{} -> Just $! accountStakedAmount acc
        _ -> Nothing

-- |Get the amount that is staked on the account.
getStakedAmount :: (Monad m) => PersistentAccount av -> m Amount
getStakedAmount acc = return $! accountStakedAmount acc

-- |Get the amount that is locked in scheduled releases on the account.
getLockedAmount :: (Monad m) => PersistentAccount av -> m Amount
getLockedAmount acc = do
    let ed = enduringData acc
    return $! paedLockedAmount ed

-- | Get the current public account available balance.
-- This accounts for lock-up and staked amounts.
-- @available = total - max locked staked@
getAvailableAmount :: (Monad m) => PersistentAccount av -> m Amount
getAvailableAmount acc = do
    let ed = enduringData acc
    return $! accountAmount acc - max (accountStakedAmount acc) (paedLockedAmount ed)

-- |Get the next account nonce for transactions from this account.
getNonce :: (Monad m) => PersistentAccount av -> m Nonce
getNonce = pure . accountNonce

-- |Determine if a given operation is permitted for the account.
--
-- * For 'AllowedEncryptedTransfers' the account may only have 1 credential.
--
-- * For 'AllowedMultipleCredentials' the account must have the empty encrypted balance.
isAllowed :: MonadBlobStore m => PersistentAccount av -> AccountAllowance -> m Bool
isAllowed acc AllowedEncryptedTransfers = do
    creds <- getCredentials acc
    return $! Map.size creds == 1
isAllowed acc AllowedMultipleCredentials = do
    let ed = enduringData acc
    -- We use the invariant that if the encrypted amount is present then it will be non-empty.
    case paedEncryptedAmount ed of
        Null -> return True
        Some eaRef -> isZeroPersistentAccountEncryptedAmount =<< refLoad eaRef

-- |Get the credentials deployed on the account. This map is always non-empty and (presently)
-- will have a credential at index 'initialCredentialIndex' (0) that cannot be changed.
getCredentials :: (Monad m) => PersistentAccount av -> m (Map.Map CredentialIndex RawAccountCredential)
getCredentials acc = do
    let pd = persistingData acc
    return $! pd ^. accountCredentials

-- |Get the key used to verify transaction signatures, it records the signature scheme used as well.
getVerificationKeys :: (Monad m) => PersistentAccount av -> m AccountInformation
getVerificationKeys acc = do
    let pd = persistingData acc
    return $! pd ^. accountVerificationKeys

-- |Get the current encrypted amount on the account.
getEncryptedAmount :: MonadBlobStore m => PersistentAccount av -> m AccountEncryptedAmount
getEncryptedAmount acc = do
    let ed = enduringData acc
    case paedEncryptedAmount ed of
        Null -> return initialAccountEncryptedAmount
        Some ea -> loadPersistentAccountEncryptedAmount =<< refLoad ea

-- |Get the public key used to receive encrypted amounts.
getEncryptionKey :: MonadBlobStore f => PersistentAccount av -> f AccountEncryptionKey
getEncryptionKey acc = do
    let pd = persistingData acc
    -- The use of the unsafe @unsafeEncryptionKeyFromRaw@ function here is
    -- justified because the encryption key was validated when it was
    -- created/deployed (this is part of credential validation)
    return $! unsafeEncryptionKeyFromRaw (pd ^. accountEncryptionKey)

-- |Get the release schedule for an account.
getReleaseSummary :: MonadBlobStore m => PersistentAccount av -> m AccountReleaseSummary
getReleaseSummary acc = do
    let ed = enduringData acc
    case paedReleaseSchedule ed of
        Null -> return (AccountReleaseSummary 0 [])
        Some (rsRef, _) -> toAccountReleaseSummary =<< refLoad rsRef

-- |Get the release schedule for an account.
getReleaseSchedule :: (MonadBlobStore m) => PersistentAccount 'AccountV2 -> m (TARS.AccountReleaseSchedule 'AccountV2)
getReleaseSchedule acc = do
    let ed = enduringData acc
    TARS.fromAccountReleaseScheduleV1 <$> case paedReleaseSchedule ed of
        Null -> return TARSV1.emptyAccountReleaseSchedule
        Some (rsRef, total) -> getAccountReleaseSchedule total =<< refLoad rsRef

-- |Get the timestamp at which the next scheduled release will occur (if any).
getNextReleaseTimestamp :: MonadBlobStore m => PersistentAccount av -> m (Maybe Timestamp)
getNextReleaseTimestamp acc = do
    let ed = enduringData acc
    case paedReleaseSchedule ed of
        Null -> return Nothing
        Some (rsRef, _) -> nextReleaseTimestamp <$!> refLoad rsRef

-- |Get the baker (if any) attached to an account.
getBaker :: (MonadBlobStore m, IsAccountVersion av, AVSupportsDelegation av) => PersistentAccount av -> m (Maybe (AccountBaker av))
getBaker acc = do
    let ed = enduringData acc
    case paedStake ed of
        PersistentAccountStakeEnduringBaker{..} -> do
            abi <- refLoad paseBakerInfo
            let !bkr =
                    AccountBaker
                        { _stakedAmount = accountStakedAmount acc,
                          _stakeEarnings = paseBakerRestakeEarnings,
                          _accountBakerInfo = abi,
                          _bakerPendingChange = PendingChangeEffectiveV1 <$> paseBakerPendingChange
                        }
            return $ Just bkr
        _ -> return Nothing

-- |Get a reference to the baker info (if any) attached to an account.
getBakerInfoRef ::
    (MonadBlobStore m) =>
    PersistentAccount av ->
    m (Maybe (PersistentBakerInfoEx av))
getBakerInfoRef acc = do
    let ed = enduringData acc
    case paedStake ed of
        PersistentAccountStakeEnduringBaker{..} -> return $ Just paseBakerInfo
        _ -> return Nothing

-- |Get the baker and baker info reference (if any) attached to the account.
getBakerAndInfoRef :: (MonadBlobStore m, IsAccountVersion av, AVSupportsDelegation av) => PersistentAccount av -> m (Maybe (AccountBaker av, PersistentBakerInfoEx av))
getBakerAndInfoRef acc = do
    let ed = enduringData acc
    case paedStake ed of
        PersistentAccountStakeEnduringBaker{..} -> do
            bi <- refLoad paseBakerInfo
            let !bkr =
                    AccountBaker
                        { _stakedAmount = accountStakedAmount acc,
                          _stakeEarnings = paseBakerRestakeEarnings,
                          _accountBakerInfo = bi,
                          _bakerPendingChange = PendingChangeEffectiveV1 <$> paseBakerPendingChange
                        }
            return $ Just (bkr, paseBakerInfo)
        _ -> return Nothing

-- |Get the delegator (if any) attached to the account.
getDelegator :: (MonadBlobStore m, AVSupportsDelegation av) => PersistentAccount av -> m (Maybe (AccountDelegation av))
getDelegator acc = do
    let ed = enduringData acc
    case paedStake ed of
        PersistentAccountStakeEnduringDelegator{..} -> do
            let !del =
                    AccountDelegationV1
                        { _delegationIdentity = paseDelegatorId,
                          _delegationStakedAmount = accountStakedAmount acc,
                          _delegationStakeEarnings = paseDelegatorRestakeEarnings,
                          _delegationTarget = paseDelegatorTarget,
                          _delegationPendingChange = PendingChangeEffectiveV1 <$> paseDelegatorPendingChange
                        }
            return $ Just del
        _ -> return Nothing

-- |Get the baker or stake delegation information attached to an account.
getStake ::
    (MonadBlobStore m, IsAccountVersion av, AVSupportsDelegation av) =>
    PersistentAccount av ->
    m (AccountStake av)
getStake acc = do
    let ed = enduringData acc
    persistentToAccountStake (paedStake ed) (accountStakedAmount acc)

-- |Determine if an account has stake as a baker or delegator.
hasStake :: PersistentAccount av -> Bool
hasStake acc = case paedStake (enduringData acc) of
    PersistentAccountStakeEnduringNone -> False
    _ -> True

-- |Get details about an account's stake.
getStakeDetails :: (MonadBlobStore m, AVSupportsDelegation av) => PersistentAccount av -> m (StakeDetails av)
getStakeDetails acc = do
    let ed = enduringData acc
    return $! case paedStake ed of
        PersistentAccountStakeEnduringBaker{..} ->
            StakeDetailsBaker
                { sdStakedCapital = accountStakedAmount acc,
                  sdRestakeEarnings = paseBakerRestakeEarnings,
                  sdPendingChange = PendingChangeEffectiveV1 <$> paseBakerPendingChange
                }
        PersistentAccountStakeEnduringDelegator{..} ->
            StakeDetailsDelegator
                { sdStakedCapital = accountStakedAmount acc,
                  sdRestakeEarnings = paseDelegatorRestakeEarnings,
                  sdPendingChange = PendingChangeEffectiveV1 <$> paseDelegatorPendingChange,
                  sdDelegationTarget = paseDelegatorTarget
                }
        PersistentAccountStakeEnduringNone -> StakeDetailsNone

-- ** Updates

-- |Apply account updates to an account. It is assumed that the address in
-- account updates and account are the same.
updateAccount :: (MonadBlobStore m) => AccountUpdate -> PersistentAccount 'AccountV2 -> m (PersistentAccount 'AccountV2)
updateAccount !upd !acc0 = do
    let ed0 = enduringData acc0
    (ed1, enduringRehash1, additionalLocked) <- case upd ^. auReleaseSchedule of
        Just l -> do
            let newLockedFunds = amountToDelta $ foldl' (+) 0 (concatMap (\(values, _) -> map snd values) l)
            (rData, oldLockedAmt) <- case paedReleaseSchedule ed0 of
                Null -> return (emptyAccountReleaseSchedule, 0)
                Some (rsRef, lockedAmt) -> (,lockedAmt) <$> refLoad rsRef
            newReleaseSchedule <- foldlM (flip addReleases) rData l
            releaseScheduleRef <-
                if isEmptyAccountReleaseSchedule newReleaseSchedule
                    then return Null
                    else do
                        !ref <- refMake newReleaseSchedule
                        let !lockedAmt = applyAmountDelta newLockedFunds oldLockedAmt
                        return (Some (ref, lockedAmt))
            let ed1 = ed0{paedReleaseSchedule = releaseScheduleRef}
            return (ed1, True, newLockedFunds)
        Nothing -> return (ed0, False, 0)
    (ed2, enduringRehash2) <- case upd ^. auEncrypted of
        Just encUpd -> do
            oldEncAmount <- case paedEncryptedAmount ed1 of
                Null -> initialPersistentAccountEncryptedAmount
                Some eaRef -> refLoad eaRef
            newEncryptedAmount <-
                ( case encUpd of
                    Add{..} -> addIncomingEncryptedAmount newAmount
                    ReplaceUpTo{..} -> replaceUpTo aggIndex newAmount
                    AddSelf{..} -> addToSelfEncryptedAmount newAmount
                    )
                    oldEncAmount
            isInitial <- isInitialPersistentAccountEncryptedAmount newEncryptedAmount
            encryptedAmountRef <-
                if isInitial
                    then return Null
                    else Some <$> refMake newEncryptedAmount
            let ed2 = ed1{paedEncryptedAmount = encryptedAmountRef}
            return (ed2, True)
        Nothing -> return (ed1, enduringRehash1)
    acc1 <-
        if enduringRehash2
            then do
                edRef <- refMake =<< rehashAccountEnduringData ed2
                return $! acc0{accountEnduringData = edRef}
            else return acc0
    let acc2 = case upd ^. auNonce of
            Nothing -> acc1
            Just n -> acc1{accountNonce = n}
    let !acc3 =
            acc2
                { accountAmount =
                    applyAmountDelta (additionalLocked + upd ^. auAmount . non 0) (accountAmount acc2)
                }
    return acc3

-- |Helper function. Apply an update to the 'PersistentAccountEnduringData' on an account,
-- recomputing the hash.
updateEnduringData ::
    (MonadBlobStore m) =>
    (PersistentAccountEnduringData 'AccountV2 -> m (PersistentAccountEnduringData 'AccountV2)) ->
    PersistentAccount 'AccountV2 ->
    m (PersistentAccount 'AccountV2)
updateEnduringData f acc = do
    let ed = enduringData acc
    newEnduring <- refMake =<< rehashAccountEnduringData =<< f ed
    return $! acc{accountEnduringData = newEnduring}

-- |Apply an update to the 'PersistingAccountData' on an account, recomputing the hash.
updatePersistingData ::
    (MonadBlobStore m) =>
    (PersistingAccountData -> PersistingAccountData) ->
    PersistentAccount 'AccountV2 ->
    m (PersistentAccount 'AccountV2)
updatePersistingData f = updateEnduringData $ \ed -> do
    let pd = eagerBufferedDeref (paedPersistingData ed)
    newPersisting <- refMake $! f pd
    return $! ed{paedPersistingData = newPersisting}

-- |Helper function. Update the 'PersistentAccountStakeEnduring' component of an account.
updateStake ::
    (MonadBlobStore m) =>
    (PersistentAccountStakeEnduring 'AccountV2 -> m (PersistentAccountStakeEnduring 'AccountV2)) ->
    PersistentAccount 'AccountV2 ->
    m (PersistentAccount 'AccountV2)
updateStake f = updateEnduringData $ \ed -> do
    newStake <- f (paedStake ed)
    return $! ed{paedStake = newStake}

-- |Add or remove credentials on an account.
-- The caller must ensure the following, which are not checked:
--
-- * Any credential index that is removed must already exist.
-- * The credential with index 0 must not be removed.
-- * Any credential index that is added must not exist after the removals take effect.
-- * At least one credential remains after all removals and additions.
-- * Any new threshold is at most the number of accounts remaining (and at least 1).
updateAccountCredentials ::
    (MonadBlobStore m) =>
    -- |Credentials to remove
    [CredentialIndex] ->
    -- |Credentials to add
    Map.Map CredentialIndex AccountCredential ->
    -- |New account threshold
    AccountThreshold ->
    -- |Account to update
    PersistentAccount 'AccountV2 ->
    m (PersistentAccount 'AccountV2)
updateAccountCredentials cuRemove cuAdd cuAccountThreshold =
    updatePersistingData (updateCredentials cuRemove cuAdd cuAccountThreshold)

-- |Optionally update the verification keys and signature threshold for an account.
-- Precondition: The credential with given credential index exists.
updateAccountCredentialKeys ::
    (MonadBlobStore m) =>
    -- |Credential to update
    CredentialIndex ->
    -- |New public keys
    CredentialPublicKeys ->
    -- |Account to update
    PersistentAccount 'AccountV2 ->
    m (PersistentAccount 'AccountV2)
updateAccountCredentialKeys credIndex credKeys = updatePersistingData (updateCredentialKeys credIndex credKeys)

-- |Add an amount to the account's balance.
addAmount :: (Monad m) => Amount -> PersistentAccount av -> m (PersistentAccount av)
addAmount !amt acc = return $! acc{accountAmount = accountAmount acc + amt}

-- |Add a baker to an account for account version 1.
-- This will replace any existing staking information on the account.
addBakerV1 ::
    (MonadBlobStore m) =>
    -- |Extended baker info
    BakerInfoEx 'AccountV2 ->
    -- |Baker's equity capital
    Amount ->
    -- |Whether earnings are restaked
    Bool ->
    -- |Account to add baker to
    PersistentAccount 'AccountV2 ->
    m (PersistentAccount 'AccountV2)
addBakerV1 binfo stake restake acc = do
    let ed = enduringData acc
    binfoRef <- refMake $! binfo
    let baker =
            PersistentAccountStakeEnduringBaker
                { paseBakerRestakeEarnings = restake,
                  paseBakerInfo = binfoRef,
                  paseBakerPendingChange = NoChange
                }
    newEnduring <- refMake =<< rehashAccountEnduringData ed{paedStake = baker}
    return $!
        acc
            { accountStakedAmount = stake,
              accountEnduringData = newEnduring
            }

-- |Add a delegator to an account.
-- This will replace any existing staking information on the account.
addDelegator ::
    (MonadBlobStore m) =>
    AccountDelegation 'AccountV2 ->
    PersistentAccount 'AccountV2 ->
    m (PersistentAccount 'AccountV2)
addDelegator AccountDelegationV1{..} acc = do
    let ed = enduringData acc
    let del =
            PersistentAccountStakeEnduringDelegator
                { paseDelegatorId = _delegationIdentity,
                  paseDelegatorRestakeEarnings = _delegationStakeEarnings,
                  paseDelegatorTarget = _delegationTarget,
                  paseDelegatorPendingChange = NoChange
                }
    newEnduring <- refMake =<< rehashAccountEnduringData ed{paedStake = del}
    return $!
        acc
            { accountStakedAmount = _delegationStakedAmount,
              accountEnduringData = newEnduring
            }

-- |Update the pool info on a baker account.
-- This MUST only be called with an account that is a baker.
updateBakerPoolInfo ::
    (MonadBlobStore m) =>
    BakerPoolInfoUpdate ->
    PersistentAccount 'AccountV2 ->
    m (PersistentAccount 'AccountV2)
updateBakerPoolInfo upd = updateEnduringData $ \ed -> case paedStake ed of
    baker@PersistentAccountStakeEnduringBaker{} -> do
        oldInfo <- refLoad (paseBakerInfo baker)
        let newInfo = oldInfo & bieBakerPoolInfo %~ applyBakerPoolInfoUpdate upd
        newInfoRef <- refMake $! newInfo
        return $! ed{paedStake = baker{paseBakerInfo = newInfoRef}}
    PersistentAccountStakeEnduringDelegator{} ->
        error "updateBakerPoolInfo invariant violation: account is not a baker"
    PersistentAccountStakeEnduringNone ->
        error "updateBakerPoolInfo invariant violation: account is not a baker"

-- |Set the baker keys on a baker account.
-- This MUST only be called with an account that is a baker.
setBakerKeys ::
    (MonadBlobStore m) =>
    BakerKeyUpdate ->
    PersistentAccount 'AccountV2 ->
    m (PersistentAccount 'AccountV2)
setBakerKeys upd = updateStake $ \case
    baker@PersistentAccountStakeEnduringBaker{} -> do
        oldInfo <- refLoad (paseBakerInfo baker)
        let newInfo =
                oldInfo
                    & bieBakerInfo
                        %~ ( (bakerAggregationVerifyKey .~ bkuAggregationKey upd)
                                . (bakerSignatureVerifyKey .~ bkuSignKey upd)
                                . (bakerElectionVerifyKey .~ bkuElectionKey upd)
                           )
        newInfoRef <- refMake $! newInfo
        return $! baker{paseBakerInfo = newInfoRef}
    PersistentAccountStakeEnduringDelegator{} ->
        error "setBakerKeys invariant violation: account is not a baker"
    PersistentAccountStakeEnduringNone ->
        error "setBakerKeys invariant violation: account is not a baker"

-- |Set the stake of a baker or delegator account.
-- This MUST only be called with an account that is either a baker or delegator.
-- This does no check that the staked amount is sensible, and has no effect on pending changes.
setStake ::
    (Monad m) =>
    Amount ->
    PersistentAccount av ->
    m (PersistentAccount av)
setStake newStake acc = return $! acc{accountStakedAmount = newStake}

-- |Set whether a baker or delegator account restakes its earnings.
-- This MUST only be called with an account that is either a baker or delegator.
setRestakeEarnings ::
    (MonadBlobStore m) =>
    Bool ->
    PersistentAccount 'AccountV2 ->
    m (PersistentAccount 'AccountV2)
setRestakeEarnings newRestake =
    updateStake $
        return . \case
            baker@PersistentAccountStakeEnduringBaker{} ->
                baker{paseBakerRestakeEarnings = newRestake}
            del@PersistentAccountStakeEnduringDelegator{} ->
                del{paseDelegatorRestakeEarnings = newRestake}
            PersistentAccountStakeEnduringNone -> error "setRestakeEarnings invariant violation: account is not a baker or delegator"

-- |Set the pending change on baker or delegator account.
-- This MUST only be called with an account that is either a baker or delegator.
setStakePendingChange ::
    (MonadBlobStore m) =>
    StakePendingChange 'AccountV2 ->
    PersistentAccount 'AccountV2 ->
    m (PersistentAccount 'AccountV2)
setStakePendingChange newPC =
    updateStake $
        return . \case
            baker@PersistentAccountStakeEnduringBaker{} ->
                baker{paseBakerPendingChange = newPC'}
            del@PersistentAccountStakeEnduringDelegator{} ->
                del{paseDelegatorPendingChange = newPC'}
            PersistentAccountStakeEnduringNone -> error "setStakePendingChange invariant violation: account is not a baker or delegator"
  where
    newPC' = pendingChangeEffectiveTimestamp <$> newPC

-- |Set the target of a delegating account.
-- This MUST only be called with an account that is a delegator.
setDelegationTarget ::
    (MonadBlobStore m) =>
    DelegationTarget ->
    PersistentAccount 'AccountV2 ->
    m (PersistentAccount 'AccountV2)
setDelegationTarget newTarget =
    updateStake $
        return . \case
            del@PersistentAccountStakeEnduringDelegator{} ->
                del{paseDelegatorTarget = newTarget}
            PersistentAccountStakeEnduringBaker{} ->
                error "setDelegationTarget invariant violation: account is not a delegator"
            PersistentAccountStakeEnduringNone ->
                error "setDelegationTarget invariant violation: account is not a delegator"

-- |Remove any staking on an account.
removeStaking ::
    (MonadBlobStore m) =>
    PersistentAccount 'AccountV2 ->
    m (PersistentAccount 'AccountV2)
removeStaking acc0 = do
    acc1 <- updateStake (const $ return PersistentAccountStakeEnduringNone) acc0
    return $! acc1{accountStakedAmount = 0}

-- |Set the commission rates on a baker account.
-- This MUST only be called with an account that is a baker.
setCommissionRates ::
    (MonadBlobStore m) =>
    CommissionRates ->
    PersistentAccount 'AccountV2 ->
    m (PersistentAccount 'AccountV2)
setCommissionRates rates = updateStake $ \case
    baker@PersistentAccountStakeEnduringBaker{} -> do
        oldInfo <- refLoad (paseBakerInfo baker)
        let newInfo = oldInfo & bieBakerPoolInfo . poolCommissionRates .~ rates
        newInfoRef <- refMake $! newInfo
        return $! baker{paseBakerInfo = newInfoRef}
    PersistentAccountStakeEnduringDelegator{} ->
        error "setCommissionRates invariant violation: account is not a baker"
    PersistentAccountStakeEnduringNone ->
        error "setCommissionRates invariant violation: account is not a baker"

-- |Unlock scheduled releases on an account up to and including the given timestamp.
-- This returns the next timestamp at which a release is scheduled for the account, if any,
-- as well as the updated account.
unlockReleases ::
    (MonadBlobStore m) =>
    Timestamp ->
    PersistentAccount 'AccountV2 ->
    m (Maybe Timestamp, PersistentAccount 'AccountV2)
unlockReleases ts acc = do
    let ed = enduringData acc
    case paedReleaseSchedule ed of
        Null -> return (Nothing, acc)
        Some (oldRSRef, oldLockedAmt) -> do
            oldRS <- refLoad oldRSRef
            (unlockedAmt, nextTimestamp, newRS) <- unlockAmountsUntil ts oldRS
            newLocked <-
                if isEmptyAccountReleaseSchedule newRS
                    then return Null
                    else do
                        newRSRef <- refMake newRS
                        (return . Some) $!! (newRSRef, oldLockedAmt - unlockedAmt)
            newEnduring <- refMake =<< rehashAccountEnduringData ed{paedReleaseSchedule = newLocked}
            let !newAcc = acc{accountEnduringData = newEnduring}
            return (nextTimestamp, newAcc)

-- ** Creation

-- |Make a 'PersistentAccount' from an 'Transient.Account'.
makePersistentAccount :: (MonadBlobStore m) => Transient.Account 'AccountV2 -> m (PersistentAccount 'AccountV2)
makePersistentAccount Transient.Account{..} = do
    paedPersistingData <- refMake $! _unhashed _accountPersisting
    (accountStakedAmount, !paedStake) <- case _accountStaking of
        AccountStakeNone -> return (0, PersistentAccountStakeEnduringNone)
        AccountStakeBaker AccountBaker{..} -> do
            paseBakerInfo <- refMake _accountBakerInfo
            let baker =
                    PersistentAccountStakeEnduringBaker
                        { paseBakerRestakeEarnings = _stakeEarnings,
                          paseBakerPendingChange = pendingChangeEffectiveTimestamp <$> _bakerPendingChange,
                          ..
                        }
            return (_stakedAmount, baker)
        AccountStakeDelegate AccountDelegationV1{..} -> do
            let del =
                    PersistentAccountStakeEnduringDelegator
                        { paseDelegatorRestakeEarnings = _delegationStakeEarnings,
                          paseDelegatorId = _delegationIdentity,
                          paseDelegatorTarget = _delegationTarget,
                          paseDelegatorPendingChange = pendingChangeEffectiveTimestamp <$> _delegationPendingChange
                        }
            return (_delegationStakedAmount, del)
    paedEncryptedAmount <- do
        ea <- storePersistentAccountEncryptedAmount _accountEncryptedAmount
        isInit <- isInitialPersistentAccountEncryptedAmount ea
        if isInit
            then return Null
            else Some <$!> refMake ea
    paedReleaseSchedule <- do
        rs <- makePersistentAccountReleaseSchedule (TARS.theAccountReleaseSchedule _accountReleaseSchedule)
        if isEmptyAccountReleaseSchedule rs
            then return Null
            else do
                rsRef <- refMake $! rs
                let !lockedBal = _accountReleaseSchedule ^. TARS.totalLockedUpBalance
                return (Some (rsRef, lockedBal))
    accountEnduringData <-
        refMake
            =<< makeAccountEnduringData
                paedPersistingData
                paedEncryptedAmount
                paedReleaseSchedule
                paedStake
    return $!
        PersistentAccount
            { accountNonce = _accountNonce,
              accountAmount = _accountAmount,
              ..
            }

-- |Create an empty account with the given public key, address and credential.
newAccount ::
    (MonadBlobStore m) =>
    GlobalContext ->
    AccountAddress ->
    AccountCredential ->
    m (PersistentAccount 'AccountV2)
newAccount cryptoParams _accountAddress credential = do
    let creds = Map.singleton initialCredentialIndex credential
    let newPData =
            PersistingAccountData
                { _accountEncryptionKey = toRawEncryptionKey (makeEncryptionKey cryptoParams (credId credential)),
                  _accountCredentials = toRawAccountCredential <$> creds,
                  _accountVerificationKeys = getAccountInformation 1 creds,
                  _accountRemovedCredentials = emptyHashedRemovedCredentials,
                  ..
                }
    paedPersistingData <- refMake newPData
    accountEnduringData <-
        refMake
            =<< makeAccountEnduringData paedPersistingData Null Null PersistentAccountStakeEnduringNone
    return $!
        PersistentAccount
            { accountNonce = minNonce,
              accountAmount = 0,
              accountStakedAmount = 0,
              ..
            }

-- |Make a persistent account from a genesis account.
-- The data is immediately flushed to disc and cached.
makeFromGenesisAccount ::
    forall pv m.
    (MonadBlobStore m, IsProtocolVersion pv, AccountVersionFor pv ~ 'AccountV2) =>
    SProtocolVersion pv ->
    GlobalContext ->
    ChainParameters pv ->
    GenesisAccount ->
    m (PersistentAccount 'AccountV2)
makeFromGenesisAccount spv cryptoParams chainParameters GenesisAccount{..} = do
    paedPersistingData <-
        refMakeFlushed $
            PersistingAccountData
                { _accountEncryptionKey =
                    toRawEncryptionKey $
                        makeEncryptionKey cryptoParams $
                            credId $
                                gaCredentials Map.! initialCredentialIndex,
                  _accountCredentials = toRawAccountCredential <$> gaCredentials,
                  _accountVerificationKeys = getAccountInformation gaThreshold gaCredentials,
                  _accountRemovedCredentials = emptyHashedRemovedCredentials,
                  _accountAddress = gaAddress
                }

    (accountStakedAmount, stakeEnduring) <- case gaBaker of
        Nothing -> return (0, PersistentAccountStakeEnduringNone)
        Just baker -> do
            paseBakerInfo <- refMakeFlushed $ genesisBakerInfoEx spv chainParameters baker
            let enduringBaker =
                    PersistentAccountStakeEnduringBaker
                        { paseBakerRestakeEarnings = True,
                          paseBakerPendingChange = NoChange,
                          ..
                        }
            return (gbStake baker, enduringBaker)

    accountEnduringData <-
        refMakeFlushed
            =<< makeAccountEnduringData paedPersistingData Null Null stakeEnduring
    return $!
        PersistentAccount
            { accountNonce = minNonce,
              accountAmount = gaBalance,
              ..
            }

-- ** Migration

-- |Migration for 'PersistentAccountEnduringData'. Only supports 'AccountV2'.
migrateEnduringData ::
    SupportMigration m t =>
    PersistentAccountEnduringData 'AccountV2 ->
    t m (PersistentAccountEnduringData 'AccountV2)
migrateEnduringData ed = do
    paedPersistingData <- migrateEagerBufferedRef return (paedPersistingData ed)
    paedEncryptedAmount <- forM (paedEncryptedAmount ed) $ migrateReference migratePersistentEncryptedAmount
    paedReleaseSchedule <- forM (paedReleaseSchedule ed) $ \(oldRSRef, lockedAmt) -> do
        newRSRef <- migrateReference migrateAccountReleaseSchedule oldRSRef
        return (newRSRef, lockedAmt)
    paedStake <- migratePersistentAccountStakeEnduring (paedStake ed)
    return $!
        PersistentAccountEnduringData
            { paedHash = paedHash ed,
              ..
            }

-- |Migration for 'PersistentAccount'. Only supports 'AccountV2'.
migratePersistentAccount ::
    ( SupportMigration m t,
      AccountVersionFor oldpv ~ 'AccountV2
    ) =>
    StateMigrationParameters oldpv pv ->
    PersistentAccount (AccountVersionFor oldpv) ->
    t m (PersistentAccount (AccountVersionFor pv))
migratePersistentAccount StateMigrationParametersTrivial acc = do
    accountEnduringData <- migrateEagerBufferedRef migrateEnduringData (accountEnduringData acc)
    return $!
        PersistentAccount
            { accountNonce = accountNonce acc,
              accountAmount = accountAmount acc,
              accountStakedAmount = accountStakedAmount acc,
              ..
            }

-- |Migration for 'PersistentAccount' from 'V0.PersistentAccount'. This supports migration from
-- 'P4' to 'P5'.
migratePersistentAccountFromV0 ::
    ( SupportMigration m t,
      AccountVersionFor oldpv ~ 'AccountV1,
      AccountVersionFor pv ~ 'AccountV2
    ) =>
    StateMigrationParameters oldpv pv ->
    V0.PersistentAccount (AccountVersionFor oldpv) ->
    t m (PersistentAccount (AccountVersionFor pv))
migratePersistentAccountFromV0 StateMigrationParametersP4ToP5{} V0.PersistentAccount{..} = do
    paedPersistingData <- migrateReference return _persistingData
    (accountStakedAmount, !paedStake) <- case _accountStake of
        V0.PersistentAccountStakeNone -> return (0, PersistentAccountStakeEnduringNone)
        V0.PersistentAccountStakeBaker bkrRef -> do
            V0.PersistentAccountBaker{..} <- lift $ refLoad bkrRef
            bkrInfoEx <- lift $ do
                bkrInfo <- refLoad _accountBakerInfo
                bkrPoolInfo <- refLoad (V0._theExtraBakerInfo _extraBakerInfo)
                return $! BakerInfoExV1 bkrInfo bkrPoolInfo
            paseBakerInfo' <- refMake bkrInfoEx
            (paseBakerInfo, _) <- refFlush paseBakerInfo'
            let baker =
                    PersistentAccountStakeEnduringBaker
                        { paseBakerRestakeEarnings = _stakeEarnings,
                          paseBakerPendingChange = pendingChangeEffectiveTimestamp <$> _bakerPendingChange,
                          ..
                        }
            return (_stakedAmount, baker)
        V0.PersistentAccountStakeDelegate dlgRef -> do
            AccountDelegationV1{..} <- lift $ refLoad dlgRef
            let del =
                    PersistentAccountStakeEnduringDelegator
                        { paseDelegatorRestakeEarnings = _delegationStakeEarnings,
                          paseDelegatorId = _delegationIdentity,
                          paseDelegatorTarget = _delegationTarget,
                          paseDelegatorPendingChange = pendingChangeEffectiveTimestamp <$> _delegationPendingChange
                        }
            return (_delegationStakedAmount, del)
    paedEncryptedAmount <- do
        mea <- lift $ do
            ea <- refLoad _accountEncryptedAmount
            isInit <- isInitialPersistentAccountEncryptedAmount ea
            return $ if isInit then Null else Some ea
        forM mea $ \ea -> do
            newEA <- migratePersistentEncryptedAmount ea
            refMake $! newEA
    paedReleaseSchedule <- do
        mrs <- lift $ do
            rs <- refLoad _accountReleaseSchedule
            return $ if ARSV0.isEmptyAccountReleaseSchedule rs then Null else Some rs
        forM mrs $ \rs -> do
            newRS <- migrateAccountReleaseScheduleFromV0 rs
            rsRef <- refMake $! newRS
            return (rsRef, ARSV0.releaseScheduleLockedBalance rs)
    (accountEnduringData, _) <-
        refFlush
            =<< refMake
            =<< makeAccountEnduringData
                paedPersistingData
                paedEncryptedAmount
                paedReleaseSchedule
                paedStake
    return $!
        PersistentAccount
            { accountNonce = _accountNonce,
              accountAmount = _accountAmount,
              ..
            }

-- ** Serialization

-- |Serialize an account. The serialization format may depend on the protocol version.
--
-- This format allows accounts to be stored in a reduced format by
-- eliding (some) data that can be inferred from context, or is
-- the default value.  Note that there can be multiple representations
-- of the same account.
serializeAccount :: (MonadBlobStore m, MonadPut m) => GlobalContext -> PersistentAccount 'AccountV2 -> m ()
serializeAccount cryptoParams acc@PersistentAccount{..} = do
    let ed = enduringData acc
    let PersistingAccountData{..} = persistingData acc
    let initialCredId =
            credId
                ( Map.findWithDefault
                    (error "Account missing initial credential")
                    initialCredentialIndex
                    _accountCredentials
                )
        asfExplicitAddress = _accountAddress /= addressFromRegIdRaw initialCredId
        -- There is an opportunity for improvement here. There is no need to go
        -- through the deserialized key. The way the encryption key is formed is
        -- that the first half is the generator, the second half is the credId.
        -- So we could just concatenate them. This requires a bit of scaffolding
        -- to get the right component out of cryptoParams, so it is not yet
        -- done.
        asfExplicitEncryptionKey = _accountEncryptionKey /= toRawEncryptionKey (makeEncryptionKey cryptoParams (unsafeCredIdFromRaw initialCredId))
        (asfMultipleCredentials, putCredentials) = case Map.toList _accountCredentials of
            [(i, cred)] | i == initialCredentialIndex -> (False, put cred)
            _ -> (True, putSafeMapOf put put _accountCredentials)
        asfThresholdIsOne = aiThreshold _accountVerificationKeys == 1
        asfHasRemovedCredentials = _accountRemovedCredentials ^. unhashed /= EmptyRemovedCredentials
    (asfExplicitEncryptedAmount, putEA) <- do
        case paedEncryptedAmount ed of
            Null -> return (False, return ())
            Some aeaRef -> do
                aea <- refLoad aeaRef
                putAccountEncryptedAmountV0 aea <&> \case
                    Nothing -> (False, return ())
                    Just p -> (True, p)
    (asfExplicitReleaseSchedule, putRS) <- do
        case paedReleaseSchedule ed of
            Null -> return (False, return ())
            Some (rsRef, _) -> do
                rs <- refLoad rsRef
                if isEmptyAccountReleaseSchedule rs
                    then return (False, return ())
                    else (True,) <$> serializeAccountReleaseSchedule rs
    let asfHasBakerOrDelegation = hasStake acc
    stake <- getStake acc
    liftPut $ do
        put AccountSerializationFlags{..}
        when asfExplicitAddress $ put _accountAddress
        when asfExplicitEncryptionKey $ put _accountEncryptionKey
        unless asfThresholdIsOne $ put (aiThreshold _accountVerificationKeys)
        putCredentials
        when asfHasRemovedCredentials $ put (_accountRemovedCredentials ^. unhashed)
        put accountNonce
        put accountAmount
        putEA
        putRS
        when asfHasBakerOrDelegation $ serializeAccountStake stake

-- ** Conversion

-- |Converts an account to a transient (i.e. in memory) account. (Used for testing.)
toTransientAccount :: (MonadBlobStore m) => PersistentAccount 'AccountV2 -> m (Transient.Account 'AccountV2)
toTransientAccount acc = do
    let _accountPersisting = makeHashed $ persistingData acc
    _accountEncryptedAmount <- getEncryptedAmount acc
    _accountReleaseSchedule <- getReleaseSchedule acc
    _accountStaking <- getStake acc
    return $
        Transient.Account
            { _accountNonce = accountNonce acc,
              _accountAmount = accountAmount acc,
              ..
            }
