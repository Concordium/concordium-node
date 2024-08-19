module Concordium.GlobalState.Persistent.Account.MigrationStateInterface where

import Concordium.Types
import Concordium.Types.Execution

-- | This class provides functionality used during account migration. Much of this functionality
--  is dependent on the protocol versions involved in the migration. This interface is used when
--  migrating one particular account.
class AccountMigration (av :: AccountVersion) m | m -> av where
    -- | Add the current account to the set of accounts that should be considered in
    --  pre-pre-cooldown as part of migration. This only has an effect when transitioning from a
    --  protocol version that does not support flexible cooldown to one that does.
    addAccountInPrePreCooldown :: m ()

    -- | Query if the given 'BakerId' is set to be removed in this migration.
    --  (The result is unspecified if the 'BakerId' was not a baker prior to migration.)
    isBakerRemoved :: BakerId -> m Bool

    -- | Record that a delegator is retained, delegating a specified amount to a delegation target.
    --  The delegator must not already have been retained. This MUST be called for every delegator
    --  that remains a delegator after migration when transitioning from a protocol version that
    --  does not support flexible delegation to one that does. Outside of such a transition, this
    --  has no effect. If the target is a baker, then the baker must be one that is retained
    --  (i.e. it was previously a baker and has not been removed in the migration).
    retainDelegator :: (AVSupportsDelegation av) => DelegatorId -> Amount -> DelegationTarget -> m ()

-- | This class provides functionality used during account migration. This interface is used when
--  migrating the entire account table.
class (AccountMigration av m) => AccountsMigration (av :: AccountVersion) m | m -> av where
    -- | Progress to the next sequential account index.
    nextAccount :: m ()
