module Concordium.GlobalState.Persistent.Account.Structure where

import Data.Maybe

import Concordium.Types
import Concordium.Types.Accounts
import Concordium.Types.Execution

-- |An update to a 'BakerPool', indicating the components that are to be replaced.
data BakerPoolInfoUpdate = BakerPoolInfoUpdate
    { updOpenForDelegation :: !(Maybe OpenStatus),
      updMetadataURL :: !(Maybe UrlText),
      updTransactionFeeCommission :: !(Maybe AmountFraction),
      updBakingRewardCommission :: !(Maybe AmountFraction),
      updFinalizationRewardCommission :: !(Maybe AmountFraction)
    }
    deriving (Eq)

-- |A 'BakerPoolInfoUpdate' that makes no changes.
emptyBakerPoolInfoUpdate :: BakerPoolInfoUpdate
emptyBakerPoolInfoUpdate =
    BakerPoolInfoUpdate
        { updOpenForDelegation = Nothing,
          updMetadataURL = Nothing,
          updTransactionFeeCommission = Nothing,
          updBakingRewardCommission = Nothing,
          updFinalizationRewardCommission = Nothing
        }

-- |Use a 'BakerPoolInfoUpdate' to update a 'BakerPoolInfo'.
applyBakerPoolInfoUpdate :: BakerPoolInfoUpdate -> BakerPoolInfo -> BakerPoolInfo
applyBakerPoolInfoUpdate
    BakerPoolInfoUpdate{..}
    BakerPoolInfo{_poolCommissionRates = CommissionRates{..}, ..} =
        BakerPoolInfo
            { _poolOpenStatus = fromMaybe _poolOpenStatus updOpenForDelegation,
              _poolMetadataUrl = fromMaybe _poolMetadataUrl updMetadataURL,
              _poolCommissionRates =
                CommissionRates
                    { _finalizationCommission = fromMaybe _finalizationCommission updFinalizationRewardCommission,
                      _bakingCommission = fromMaybe _bakingCommission updBakingRewardCommission,
                      _transactionCommission = fromMaybe _transactionCommission updTransactionFeeCommission
                    }
            }

-- |Details about the stake associated with an account.
-- Compared to 'AccountStake' this omits the 'BakerInfoEx' and the 'DelegatorId'.
-- It is expected that it should be relatively efficient to query these details.
data StakeDetails av
    = StakeDetailsNone
    | StakeDetailsBaker
        { sdStakedCapital :: !Amount,
          sdRestakeEarnings :: !Bool,
          sdPendingChange :: !(StakePendingChange av)
        }
    | StakeDetailsDelegator
        { sdStakedCapital :: !Amount,
          sdRestakeEarnings :: !Bool,
          sdPendingChange :: !(StakePendingChange av),
          sdDelegationTarget :: !DelegationTarget
        }
