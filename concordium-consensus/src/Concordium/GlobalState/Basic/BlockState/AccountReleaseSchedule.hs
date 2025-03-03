{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
-- We suppress redundant constraint warnings since GHC does not detect when a constraint is used
-- for pattern matching. (See: https://gitlab.haskell.org/ghc/ghc/-/issues/20896)
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Concordium.GlobalState.Basic.BlockState.AccountReleaseSchedule where

import Data.Function
import Lens.Micro.Platform

import Concordium.Types
import Concordium.Types.Accounts
import Concordium.Types.Accounts.Releases

import qualified Concordium.GlobalState.Basic.BlockState.AccountReleaseScheduleV0 as ARSV0
import qualified Concordium.GlobalState.Basic.BlockState.AccountReleaseScheduleV1 as ARSV1
import Concordium.Types.HashableTo

-- | Release schedule on an account, parametrized by the account version.
type family AccountReleaseSchedule' (av :: AccountVersion) where
    AccountReleaseSchedule' 'AccountV0 = ARSV0.AccountReleaseSchedule
    AccountReleaseSchedule' 'AccountV1 = ARSV0.AccountReleaseSchedule
    AccountReleaseSchedule' 'AccountV2 = ARSV1.AccountReleaseSchedule
    AccountReleaseSchedule' 'AccountV3 = ARSV1.AccountReleaseSchedule
    AccountReleaseSchedule' 'AccountV4 = ARSV1.AccountReleaseSchedule

-- | Release schedule on an account, parametrized by the account version.
newtype AccountReleaseSchedule (av :: AccountVersion) = AccountReleaseSchedule
    { theAccountReleaseSchedule :: AccountReleaseSchedule' av
    }

-- | Helper to retrieve the underlying V0 release schedule assuming we know
--  that the account version @av@ has the V0 structure.
theAccountReleaseScheduleV0 ::
    forall av.
    (IsAccountVersion av, AccountStructureVersionFor av ~ 'AccountStructureV0) =>
    AccountReleaseSchedule av ->
    ARSV0.AccountReleaseSchedule
theAccountReleaseScheduleV0 = case accountVersion @av of
    SAccountV0 -> theAccountReleaseSchedule
    SAccountV1 -> theAccountReleaseSchedule

-- | Helper to retrieve the underlying V1 release schedule assuming we know
--  that the account version @av@ has the V1 structure.
theAccountReleaseScheduleV1 ::
    forall av.
    (IsAccountVersion av, AccountStructureVersionFor av ~ 'AccountStructureV1) =>
    AccountReleaseSchedule av ->
    ARSV1.AccountReleaseSchedule
theAccountReleaseScheduleV1 = case accountVersion @av of
    SAccountV2 -> theAccountReleaseSchedule
    SAccountV3 -> theAccountReleaseSchedule
    SAccountV4 -> theAccountReleaseSchedule

-- | Converse of 'theAccountReleaseScheduleV0'.
fromAccountReleaseScheduleV0 ::
    forall av.
    (IsAccountVersion av, AccountStructureVersionFor av ~ 'AccountStructureV0) =>
    ARSV0.AccountReleaseSchedule ->
    AccountReleaseSchedule av
fromAccountReleaseScheduleV0 = case accountVersion @av of
    SAccountV0 -> AccountReleaseSchedule
    SAccountV1 -> AccountReleaseSchedule

-- | Converse of 'theAccountReleaseScheduleV1'
fromAccountReleaseScheduleV1 ::
    forall av.
    (IsAccountVersion av, AccountStructureVersionFor av ~ 'AccountStructureV1) =>
    ARSV1.AccountReleaseSchedule ->
    AccountReleaseSchedule av
fromAccountReleaseScheduleV1 = case accountVersion @av of
    SAccountV2 -> AccountReleaseSchedule
    SAccountV3 -> AccountReleaseSchedule
    SAccountV4 -> AccountReleaseSchedule

instance (IsAccountVersion av) => Eq (AccountReleaseSchedule av) where
    (==) = case accountVersion @av of
        SAccountV0 -> (==) `on` theAccountReleaseSchedule
        SAccountV1 -> (==) `on` theAccountReleaseSchedule
        SAccountV2 -> (==) `on` theAccountReleaseSchedule
        SAccountV3 -> (==) `on` theAccountReleaseSchedule
        SAccountV4 -> (==) `on` theAccountReleaseSchedule

instance (IsAccountVersion av) => Show (AccountReleaseSchedule av) where
    show = case accountVersion @av of
        SAccountV0 -> show . theAccountReleaseSchedule
        SAccountV1 -> show . theAccountReleaseSchedule
        SAccountV2 -> show . theAccountReleaseSchedule
        SAccountV3 -> show . theAccountReleaseSchedule
        SAccountV4 -> show . theAccountReleaseSchedule

-- | Produce an 'AccountReleaseSummary' from an 'AccountReleaseSchedule'.
toAccountReleaseSummary :: forall av. (IsAccountVersion av) => AccountReleaseSchedule av -> AccountReleaseSummary
toAccountReleaseSummary = case accountVersion @av of
    SAccountV0 -> ARSV0.toAccountReleaseSummary . theAccountReleaseSchedule
    SAccountV1 -> ARSV0.toAccountReleaseSummary . theAccountReleaseSchedule
    SAccountV2 -> ARSV1.toAccountReleaseSummary . theAccountReleaseSchedule
    SAccountV3 -> ARSV1.toAccountReleaseSummary . theAccountReleaseSchedule
    SAccountV4 -> ARSV1.toAccountReleaseSummary . theAccountReleaseSchedule

instance (IsAccountVersion av, AccountStructureVersionFor av ~ 'AccountStructureV0) => HashableTo ARSV0.AccountReleaseScheduleHashV0 (AccountReleaseSchedule av) where
    getHash = case accountVersion @av of
        SAccountV0 -> getHash . theAccountReleaseSchedule
        SAccountV1 -> getHash . theAccountReleaseSchedule
    {-# INLINE getHash #-}

instance (IsAccountVersion av, AccountStructureVersionFor av ~ 'AccountStructureV1) => HashableTo ARSV1.AccountReleaseScheduleHashV1 (AccountReleaseSchedule av) where
    getHash = case accountVersion @av of
        SAccountV2 -> getHash . theAccountReleaseSchedule
        SAccountV3 -> getHash . theAccountReleaseSchedule
        SAccountV4 -> getHash . theAccountReleaseSchedule
    {-# INLINE getHash #-}

-- | Create an empty account release schedule
emptyAccountReleaseSchedule :: forall av. (IsAccountVersion av) => AccountReleaseSchedule av
emptyAccountReleaseSchedule = case accountVersion @av of
    SAccountV0 -> AccountReleaseSchedule ARSV0.emptyAccountReleaseSchedule
    SAccountV1 -> AccountReleaseSchedule ARSV0.emptyAccountReleaseSchedule
    SAccountV2 -> AccountReleaseSchedule ARSV1.emptyAccountReleaseSchedule
    SAccountV3 -> AccountReleaseSchedule ARSV1.emptyAccountReleaseSchedule
    SAccountV4 -> AccountReleaseSchedule ARSV1.emptyAccountReleaseSchedule

-- | Add a list of amounts to this @AccountReleaseSchedule@.
--
-- Precondition: The list of amounts MUST be non-empty and in ascending order of timestamps.
addReleases ::
    forall av.
    (IsAccountVersion av) =>
    ([(Timestamp, Amount)], TransactionHash) ->
    AccountReleaseSchedule av ->
    AccountReleaseSchedule av
addReleases = case accountVersion @av of
    SAccountV0 -> \rels (AccountReleaseSchedule ars) -> AccountReleaseSchedule (ARSV0.addReleases rels ars)
    SAccountV1 -> \rels (AccountReleaseSchedule ars) -> AccountReleaseSchedule (ARSV0.addReleases rels ars)
    SAccountV2 -> \rels (AccountReleaseSchedule ars) -> AccountReleaseSchedule (ARSV1.addReleases rels ars)
    SAccountV3 -> \rels (AccountReleaseSchedule ars) -> AccountReleaseSchedule (ARSV1.addReleases rels ars)
    SAccountV4 -> \rels (AccountReleaseSchedule ars) -> AccountReleaseSchedule (ARSV1.addReleases rels ars)

-- | Remove the amounts up to and including the given timestamp.
-- It returns the unlocked amount, maybe the next smallest timestamp for this account and the new account release schedule.
unlockAmountsUntil ::
    forall av.
    (IsAccountVersion av) =>
    Timestamp ->
    AccountReleaseSchedule av ->
    (Amount, Maybe Timestamp, AccountReleaseSchedule av)
unlockAmountsUntil = case accountVersion @av of
    SAccountV0 -> \ts (AccountReleaseSchedule ars) ->
        _3 %~ AccountReleaseSchedule $ ARSV0.unlockAmountsUntil ts ars
    SAccountV1 -> \ts (AccountReleaseSchedule ars) ->
        _3 %~ AccountReleaseSchedule $ ARSV0.unlockAmountsUntil ts ars
    SAccountV2 -> \ts (AccountReleaseSchedule ars) ->
        _3 %~ AccountReleaseSchedule $ ARSV1.unlockAmountsUntil ts ars
    SAccountV3 -> \ts (AccountReleaseSchedule ars) ->
        _3 %~ AccountReleaseSchedule $ ARSV1.unlockAmountsUntil ts ars
    SAccountV4 -> \ts (AccountReleaseSchedule ars) ->
        _3 %~ AccountReleaseSchedule $ ARSV1.unlockAmountsUntil ts ars

-- | Get the timestamp at which the next scheduled release will occur (if any).
nextReleaseTimestamp :: forall av. (IsAccountVersion av) => AccountReleaseSchedule av -> Maybe Timestamp
nextReleaseTimestamp = case accountVersion @av of
    SAccountV0 -> ARSV0.nextReleaseTimestamp . theAccountReleaseSchedule
    SAccountV1 -> ARSV0.nextReleaseTimestamp . theAccountReleaseSchedule
    SAccountV2 -> ARSV1.nextReleaseTimestamp . theAccountReleaseSchedule
    SAccountV3 -> ARSV1.nextReleaseTimestamp . theAccountReleaseSchedule
    SAccountV4 -> ARSV1.nextReleaseTimestamp . theAccountReleaseSchedule

-- | Get the total locked balance.
totalLockedUpBalance :: forall av. (IsAccountVersion av) => SimpleGetter (AccountReleaseSchedule av) Amount
totalLockedUpBalance = case accountVersion @av of
    SAccountV0 -> to theAccountReleaseSchedule . ARSV0.totalLockedUpBalance
    SAccountV1 -> to theAccountReleaseSchedule . ARSV0.totalLockedUpBalance
    SAccountV2 -> to (ARSV1.arsTotalLockedAmount . theAccountReleaseSchedule)
    SAccountV3 -> to (ARSV1.arsTotalLockedAmount . theAccountReleaseSchedule)
    SAccountV4 -> to (ARSV1.arsTotalLockedAmount . theAccountReleaseSchedule)

-- | Compute the sum of releases in the release schedule.
--  This should produce the same result as '_totalLockedUpBalance', and is provided for testing
--  purposes.
sumOfReleases :: forall av. (IsAccountVersion av) => AccountReleaseSchedule av -> Amount
sumOfReleases = case accountVersion @av of
    SAccountV0 -> ARSV0.sumOfReleases . theAccountReleaseSchedule
    SAccountV1 -> ARSV0.sumOfReleases . theAccountReleaseSchedule
    SAccountV2 -> ARSV1.sumOfReleases . theAccountReleaseSchedule
    SAccountV3 -> ARSV1.sumOfReleases . theAccountReleaseSchedule
    SAccountV4 -> ARSV1.sumOfReleases . theAccountReleaseSchedule
