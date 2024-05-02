module Concordium.GlobalState.Persistent.Account.MigrationStateInterface where

import Concordium.Types
import Concordium.Types.Execution

class AccountMigration (av :: AccountVersion) m | m -> av where
    -- | Add the current account to the set of accounts that should be considered in
    --  pre-pre-cooldown as part of migration. This only has an effect when transitioning from a
    --  protocol version that does not support flexible cooldown to one that does.
    addAccountInPrePreCooldown :: m ()

    -- | Progress to the next sequential account index.
    nextAccount :: m ()

    -- | Query if the given 'BakerId' is set to be removed in this migration.
    --  (The result is unspecified if the 'BakerId' was not a baker prior to migration.)
    isBakerRemoved :: BakerId -> m Bool

    -- | Add a delegator, delegating a specified amount to a delegation target.
    --  The delegator must not already have been added.
    addDelegator :: (AVSupportsDelegation av) => DelegatorId -> Amount -> DelegationTarget -> m ()
