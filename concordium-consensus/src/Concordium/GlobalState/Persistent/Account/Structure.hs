module Concordium.GlobalState.Persistent.Account.Structure where

import Data.Maybe

import Concordium.Types
import Concordium.Types.Accounts
import Concordium.Types.Execution

data BakerPoolInfoUpdate = BakerPoolInfoUpdate
    { updOpenForDelegation :: !(Maybe OpenStatus),
      updMetadataURL :: !(Maybe UrlText),
      updTransactionFeeCommission :: !(Maybe AmountFraction),
      updBakingRewardCommission :: !(Maybe AmountFraction),
      updFinalizationRewardCommission :: !(Maybe AmountFraction)
    }
    deriving (Eq)

emptyBakerPoolInfoUpdate :: BakerPoolInfoUpdate
emptyBakerPoolInfoUpdate =
    BakerPoolInfoUpdate
        { updOpenForDelegation = Nothing,
          updMetadataURL = Nothing,
          updTransactionFeeCommission = Nothing,
          updBakingRewardCommission = Nothing,
          updFinalizationRewardCommission = Nothing
        }

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