{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- |This module provides a mocked implementation of the block state-related monads.
-- This supports writing unit tests that define the sequence of actions that should be taken by
-- a function.
module GlobalStateMock where

import Control.Monad.Identity
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Data.Type.Equality
import qualified Data.Vector as Vec
import Type.Reflection

import Concordium.Crypto.EncryptedTransfers
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.Basic.BlockState.AccountReleaseSchedule
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.CapitalDistribution
import Concordium.GlobalState.TransactionTable (TransactionTable)
import Concordium.GlobalState.Types
import qualified Concordium.GlobalState.Wasm as GSWasm
import qualified Concordium.ID.Types as ID
import Concordium.Scheduler.Types
import Concordium.Types.Accounts
import Concordium.Types.Queries
import Concordium.Types.SeedState
import qualified Concordium.Types.UpdateQueues as UQ
import qualified Concordium.Wasm as Wasm

import Concordium.GlobalState.Account
import Concordium.GlobalState.Basic.BlockState.PoolRewards
import Concordium.ID.Parameters
import Data.Word
import GlobalStateMock.Actions

-- |Pair an action that produces a result with the expected result.
data WithResult f where
    (:->) :: f r -> r -> WithResult f

instance (Act f) => Show (WithResult f) where
    show (act :-> res) = show act ++ " :-> " ++ showRes act res

newtype MockBlockState = MockBlockState Integer
    deriving (Eq, Show)
newtype MockUpdatableBlockState = MockUpdatableBlockState Integer
    deriving (Eq, Show)
newtype MockAccount = MockAccount Integer
    deriving (Eq, Show)
newtype MockContractState (v :: Wasm.WasmVersion) = MockContractState Integer
    deriving (Eq, Show)
newtype MockBakerInfoRef = MockBakerInfoRef Integer
    deriving (Eq, Show)

-- |Mock type for 'AccountOperations'.
data AccountOperationsAction (pv :: ProtocolVersion) a where
    GetAccountCanonicalAddress :: MockAccount -> AccountOperationsAction pv AccountAddress
    GetAccountAmount :: MockAccount -> AccountOperationsAction pv Amount
    CheckAccountIsAllowed :: MockAccount -> AccountAllowance -> AccountOperationsAction pv Bool
    GetAccountAvailableAmount :: MockAccount -> AccountOperationsAction pv Amount
    GetAccountNonce :: MockAccount -> AccountOperationsAction pv Nonce
    GetAccountCredentials :: MockAccount -> AccountOperationsAction pv (Map.Map ID.CredentialIndex ID.RawAccountCredential)
    GetAccountVerificationKeys :: MockAccount -> AccountOperationsAction pv ID.AccountInformation
    GetAccountEncryptedAmount :: MockAccount -> AccountOperationsAction pv AccountEncryptedAmount
    GetAccountEncryptionKey :: MockAccount -> AccountOperationsAction pv ID.AccountEncryptionKey
    GetAccountEncryptedAmountNextIndex :: MockAccount -> AccountOperationsAction pv EncryptedAmountIndex
    GetAccountEncryptedAmountAtIndex :: MockAccount -> EncryptedAmountAggIndex -> AccountOperationsAction pv (Maybe EncryptedAmount)
    GetAccountReleaseSchedule :: MockAccount -> AccountOperationsAction pv AccountReleaseSchedule
    GetAccountBaker :: MockAccount -> AccountOperationsAction pv (Maybe (AccountBaker (AccountVersionFor pv)))
    GetAccountBakerInfoRef :: MockAccount -> AccountOperationsAction pv (Maybe MockBakerInfoRef)
    GetAccountDelegator :: MockAccount -> AccountOperationsAction pv (Maybe (AccountDelegation (AccountVersionFor pv)))
    GetAccountStake :: MockAccount -> AccountOperationsAction pv (AccountStake (AccountVersionFor pv))
    DerefBakerInfo :: MockBakerInfoRef -> AccountOperationsAction pv BakerInfo
    GetAccountHash :: MockAccount -> AccountOperationsAcciont pv AccountHash
    deriving (Typeable)

deriving instance Eq (AccountOperationsAction pv a)
deriving instance Show (AccountOperationsAction pv a)

generateAct ''AccountOperationsAction

-- |Mock type for 'ContractStateOperations'.
-- None of the operations are currently implemented.
data ContractStateOperationsAction a where

deriving instance Eq (ContractStateOperationsAction a)
deriving instance Show (ContractStateOperationsAction a)

-- |Mock type for 'BlockStateQuery'.
data BlockStateQueryAction (pv :: ProtocolVersion) a where
    GetModule :: MockBlockState -> ModuleRef -> BlockStateQueryAction pv (Maybe Wasm.WasmModule)
    GetModuleInterface :: MockBlockState -> ModuleRef -> BlockStateQueryAction pv (Maybe GSWasm.ModuleInterface)
    GetAccount :: MockBlockState -> AccountAddress -> BlockStateQueryAction pv (Maybe (AccountIndex, MockAccount))
    GetAccountByIndex :: MockBlockState -> AccountIndex -> BlockStateQueryAction pv (Maybe (AccountIndex, MockAccount))
    AccountExists :: MockBlockState -> AccountAddress -> BlockStateQueryAction pv Bool
    GetActiveBakers :: MockBlockState -> BlockStateQueryAction pv [BakerId]
    GetActiveBakersAndDelegators :: (AccountVersionFor pv ~ 'AccountV1) => MockBlockState -> BlockStateQueryAction pv ([ActiveBakerInfo' MockBakerInfoRef], [ActiveDelegatorInfo])
    GetAccountByCredId :: MockBlockState -> ID.RawCredentialRegistrationID -> BlockStateQueryAction pv (Maybe (AccountIndex, MockAccount))
    GetContractInstance :: MockBlockState -> ContractAddress -> BlockStateQueryAction pv (Maybe (InstanceInfoType MockContractState))
    GetModuleList :: MockBlockState -> BlockStateQueryAction pv [ModuleRef]
    GetAccountList :: MockBlockState -> BlockStateQueryAction pv [AccountAddress]
    GetContractInstanceList :: MockBlockState -> BlockStateQueryAction pv [ContractAddress]
    GetSeedState :: MockBlockState -> BlockStateQueryAction pv SeedState
    GetCurrentEpochBakers :: MockBlockState -> BlockStateQueryAction pv FullBakers
    GetNextEpochBakers :: MockBlockState -> BlockStateQueryAction pv FullBakers
    GetSlotBakersP1 :: MockBlockState -> Slot -> BlockStateQueryAction pv FullBakers
    GetBakerAccount :: MockBlockState -> BakerId -> BlockStateQueryAction pv (Maybe MockAccount)
    GetRewardStatus :: MockBlockState -> BlockStateQueryAction pv (RewardStatus' Epoch)
    GetTransactionOutcome :: MockBlockState -> TransactionIndex -> BlockStateQueryAction pv (Maybe TransactionSummary)
    GetTransactionOutcomesHash :: MockBlockState -> BlockStateQueryAction pv TransactionOutcomesHash
    GetStateHash :: MockBlockState -> BlockStateQueryAction pv StateHash
    GetOutcomes :: MockBlockState -> BlockStateQueryAction pv (Vec.Vector TransactionSummary)
    GetSpecialOutcomes :: MockBlockState -> BlockStateQueryAction pv (Seq.Seq SpecialTransactionOutcome)
    GetIdentityProvider :: MockBlockState -> ID.IdentityProviderIdentity -> BlockStateQueryAction pv (Maybe IpInfo)
    GetAllIdentityProviders :: MockBlockState -> BlockStateQueryAction pv [IpInfo]
    GetAnonymityRevokers :: MockBlockState -> [ID.ArIdentity] -> BlockStateQueryAction pv (Maybe [ArInfo])
    GetAllAnonymityRevokers :: MockBlockState -> BlockStateQueryAction pv [ArInfo]
    GetElectionDifficulty :: MockBlockState -> Timestamp -> BlockStateQueryAction pv ElectionDifficulty
    GetNextUpdateSequenceNumber :: MockBlockState -> UpdateType -> BlockStateQueryAction pv UpdateSequenceNumber
    GetCurrentElectionDifficulty :: MockBlockState -> BlockStateQueryAction pv ElectionDifficulty
    GetUpdates :: MockBlockState -> BlockStateQueryAction pv (UQ.Updates pv)
    GetPendingTimeParameters :: MockBlockState -> BlockStateQueryAction pv [(TransactionTime, TimeParameters (ChainParametersVersionFor pv))]
    GetPendingPoolParameters :: MockBlockState -> BlockStateQueryAction pv [(TransactionTime, PoolParameters (ChainParametersVersionFor pv))]
    GetProtocolUpdateStatus :: MockBlockState -> BlockStateQueryAction pv UQ.ProtocolUpdateStatus
    GetCryptographicParameters :: MockBlockState -> BlockStateQueryAction pv CryptographicParameters
    GetUpdateKeysCollection :: MockBlockState -> BlockStateQueryAction pv (UpdateKeysCollection (ChainParametersVersionFor pv))
    GetEnergyRate :: MockBlockState -> BlockStateQueryAction pv EnergyRate
    GetPaydayEpoch :: (AccountVersionFor pv ~ 'AccountV1) => MockBlockState -> BlockStateQueryAction pv Epoch
    GetPoolStatus :: (AccountVersionFor pv ~ 'AccountV1, ChainParametersVersionFor pv ~ 'ChainParametersV1) => MockBlockState -> Maybe BakerId -> BlockStateQueryAction pv (Maybe PoolStatus)
    GetInitialTransactionTable :: MockBlockState -> BlockStateQueryAction pv TransactionTable

deriving instance Eq (BlockStateQueryAction pv a)
deriving instance Show (BlockStateQueryAction pv a)

generateAct ''BlockStateQueryAction

-- |Mock type for 'BlockStateOperations'.
-- Note, 'bsoPutNewInstance', 'bsoModifyInstance', 'bsoPutNewModule', and 'bsoProcessPendingChanges'
-- are not supported as the arguments do not permit equality checks.
data BlockStateOperationsAction pv a where
    BsoGetModule :: MockUpdatableBlockState -> ModuleRef -> BlockStateOperationsAction pv (Maybe GSWasm.ModuleInterface)
    BsoGetAccount :: MockUpdatableBlockState -> AccountAddress -> BlockStateOperationsAction pv (Maybe (AccountIndex, MockAccount))
    BsoGetAccountIndex :: MockUpdatableBlockState -> AccountAddress -> BlockStateOperationsAction pv (Maybe AccountIndex)
    BsoGetAccountByIndex :: MockUpdatableBlockState -> AccountIndex -> BlockStateOperationsAction pv (Maybe MockAccount)
    BsoGetInstance :: MockUpdatableBlockState -> ContractAddress -> BlockStateOperationsAction pv (Maybe (InstanceInfoType MockContractState))
    BsoAddressWouldClash :: MockUpdatableBlockState -> ID.AccountAddress -> BlockStateOperationsAction pv Bool
    BsoRegIdExists :: MockUpdatableBlockState -> ID.CredentialRegistrationID -> BlockStateOperationsAction pv Bool
    BsoCreateAccount :: MockUpdatableBlockState -> GlobalContext -> AccountAddress -> ID.AccountCredential -> BlockStateOperationsAction pv (Maybe MockAccount, MockUpdatableBlockState)
    BsoModifyAccount :: MockUpdatableBlockState -> AccountUpdate -> BlockStateOperationsAction pv MockUpdatableBlockState
    BsoSetAccountCredentialKeys :: MockUpdatableBlockState -> AccountIndex -> ID.CredentialIndex -> ID.CredentialPublicKeys -> BlockStateOperationsAction pv MockUpdatableBlockState
    BsoUpdateAccountCredentials :: MockUpdatableBlockState -> AccountIndex -> [ID.CredentialIndex] -> Map.Map ID.CredentialIndex ID.AccountCredential -> ID.AccountThreshold -> BlockStateOperationsAction pv MockUpdatableBlockState
    BsoNotifyEncryptedBalanceChange :: MockUpdatableBlockState -> AmountDelta -> BlockStateOperationsAction pv MockUpdatableBlockState
    BsoGetSeedState :: MockUpdatableBlockState -> BlockStateOperationsAction pv SeedState
    BsoSetSeedState :: MockUpdatableBlockState -> SeedState -> BlockStateOperationsAction pv MockUpdatableBlockState
    BsoRotateCurrentEpochBakers :: MockUpdatableBlockState -> BlockStateOperationsAction pv MockUpdatableBlockState
    BsoSetNextEpochBakers :: (AccountVersionFor pv ~ 'AccountV1) => MockUpdatableBlockState -> [(MockBakerInfoRef, Amount)] -> BlockStateOperationsAction pv MockUpdatableBlockState
    BsoTransitionEpochBakers :: (AccountVersionFor pv ~ 'AccountV0) => MockUpdatableBlockState -> Epoch -> BlockStateOperationsAction pv MockUpdatableBlockState
    BsoGetActiveBakers :: MockUpdatableBlockState -> BlockStateOperationsAction pv [BakerId]
    BsoGetActiveBakersAndDelegators :: (AccountVersionFor pv ~ 'AccountV1) => MockUpdatableBlockState -> BlockStateOperationsAction pv ([ActiveBakerInfo' MockBakerInfoRef], [ActiveDelegatorInfo])
    BsoGetCurrentEpochBakers :: MockUpdatableBlockState -> BlockStateOperationsAction pv FullBakers
    BsoGetCurrentEpochFullBakersEx :: (AccountVersionFor pv ~ 'AccountV1) => MockUpdatableBlockState -> BlockStateOperationsAction pv FullBakersEx
    BsoGetCurrentCapitalDistribution :: (AccountVersionFor pv ~ 'AccountV1) => MockUpdatableBlockState -> BlockStateOperationsAction pv CapitalDistribution
    BsoAddBaker :: (AccountVersionFor pv ~ 'AccountV0, ChainParametersVersionFor pv ~ 'ChainParametersV0) => MockUpdatableBlockState -> AccountIndex -> BakerAdd -> BlockStateOperationsAction pv (BakerAddResult, MockUpdatableBlockState)
    BsoConfigureBaker :: (AccountVersionFor pv ~ 'AccountV1, ChainParametersVersionFor pv ~ 'ChainParametersV1) => MockUpdatableBlockState -> AccountIndex -> BakerConfigure -> BlockStateOperationsAction pv (BakerConfigureResult, MockUpdatableBlockState)
    BsoConstrainBakerCommission :: (AccountVersionFor pv ~ 'AccountV1, ChainParametersVersionFor pv ~ 'ChainParametersV1) => MockUpdatableBlockState -> AccountIndex -> CommissionRanges -> BlockStateOperationsAction pv MockUpdatableBlockState
    BsoConfigureDelegation :: (AccountVersionFor pv ~ 'AccountV1, ChainParametersVersionFor pv ~ 'ChainParametersV1) => MockUpdatableBlockState -> AccountIndex -> DelegationConfigure -> BlockStateOperationsAction pv (DelegationConfigureResult, MockUpdatableBlockState)
    BsoUpdateBakerKeys :: (AccountVersionFor pv ~ 'AccountV0) => MockUpdatableBlockState -> AccountIndex -> BakerKeyUpdate -> BlockStateOperationsAction pv (BakerKeyUpdateResult, MockUpdatableBlockState)
    BsoUpdateBakerStake :: (AccountVersionFor pv ~ 'AccountV0, ChainParametersVersionFor pv ~ 'ChainParametersV0) => MockUpdatableBlockState -> AccountIndex -> Amount -> BlockStateOperationsAction pv (BakerStakeUpdateResult, MockUpdatableBlockState)
    BsoUpdateBakerRestakeEarnings :: (AccountVersionFor pv ~ 'AccountV0) => MockUpdatableBlockState -> AccountIndex -> Bool -> BlockStateOperationsAction pv (BakerRestakeEarningsUpdateResult, MockUpdatableBlockState)
    BsoRemoveBaker :: (AccountVersionFor pv ~ 'AccountV0, ChainParametersVersionFor pv ~ 'ChainParametersV0) => MockUpdatableBlockState -> AccountIndex -> BlockStateOperationsAction pv (BakerRemoveResult, MockUpdatableBlockState)
    BsoRewardAccount :: MockUpdatableBlockState -> AccountIndex -> Amount -> BlockStateOperationsAction pv (Maybe AccountAddress, MockUpdatableBlockState)
    BsoGetBakerPoolRewardDetails :: AccountVersionFor pv ~ 'AccountV1 => MockUpdatableBlockState -> BlockStateOperationsAction pv (Map.Map BakerId BakerPoolRewardDetails)
    BsoUpdateAccruedTransactionFeesBaker :: AccountVersionFor pv ~ 'AccountV1 => MockUpdatableBlockState -> BakerId -> AmountDelta -> BlockStateOperationsAction pv MockUpdatableBlockState
    BsoMarkFinalizationAwakeBakers :: AccountVersionFor pv ~ 'AccountV1 => MockUpdatableBlockState -> [BakerId] -> BlockStateOperationsAction pv MockUpdatableBlockState
    BsoUpdateAccruedTransactionFeesPassive :: AccountVersionFor pv ~ 'AccountV1 => MockUpdatableBlockState -> AmountDelta -> BlockStateOperationsAction pv MockUpdatableBlockState
    BsoGetAccruedTransactionFeesPassive :: AccountVersionFor pv ~ 'AccountV1 => MockUpdatableBlockState -> BlockStateOperationsAction pv Amount
    BsoUpdateAccruedTransactionFeesFoundationAccount :: AccountVersionFor pv ~ 'AccountV1 => MockUpdatableBlockState -> AmountDelta -> BlockStateOperationsAction pv MockUpdatableBlockState
    BsoGetAccruedTransactionFeesFoundationAccount :: AccountVersionFor pv ~ 'AccountV1 => MockUpdatableBlockState -> BlockStateOperationsAction pv Amount
    BsoRewardFoundationAccount :: MockUpdatableBlockState -> Amount -> BlockStateOperationsAction pv MockUpdatableBlockState
    BsoGetFoundationAccount :: MockUpdatableBlockState -> BlockStateOperationsAction pv MockAccount
    BsoMint :: MockUpdatableBlockState -> MintAmounts -> BlockStateOperationsAction pv MockUpdatableBlockState
    BsoGetIdentityProvider :: MockUpdatableBlockState -> ID.IdentityProviderIdentity -> BlockStateOperationsAction pv (Maybe IpInfo)
    BsoGetAnonymityRevokers :: MockUpdatableBlockState -> [ID.ArIdentity] -> BlockStateOperationsAction pv (Maybe [ArInfo])
    BsoGetCryptoParams :: MockUpdatableBlockState -> BlockStateOperationsAction pv CryptographicParameters
    BsoGetPaydayEpoch :: (AccountVersionFor pv ~ 'AccountV1) => MockUpdatableBlockState -> BlockStateOperationsAction pv Epoch
    BsoGetPaydayMintRate :: (AccountVersionFor pv ~ 'AccountV1) => MockUpdatableBlockState -> BlockStateOperationsAction pv MintRate
    BsoSetPaydayEpoch :: (AccountVersionFor pv ~ 'AccountV1) => MockUpdatableBlockState -> Epoch -> BlockStateOperationsAction pv MockUpdatableBlockState
    BsoSetPaydayMintRate :: (AccountVersionFor pv ~ 'AccountV1) => MockUpdatableBlockState -> MintRate -> BlockStateOperationsAction pv MockUpdatableBlockState
    BsoSetTransactionOutcomes :: MockUpdatableBlockState -> [TransactionSummary] -> BlockStateOperationsAction pv MockUpdatableBlockState
    BsoAddSpecialTransactionOutcome :: MockUpdatableBlockState -> SpecialTransactionOutcome -> BlockStateOperationsAction pv MockUpdatableBlockState
    BsoProcessUpdateQueues :: MockUpdatableBlockState -> Timestamp -> BlockStateOperationsAction pv (Map.Map TransactionTime (UpdateValue (ChainParametersVersionFor pv)), MockUpdatableBlockState)
    BsoProcessReleaseSchedule :: MockUpdatableBlockState -> Timestamp -> BlockStateOperationsAction pv MockUpdatableBlockState
    BsoGetUpdateKeyCollection :: MockUpdatableBlockState -> BlockStateOperationsAction pv (UpdateKeysCollection (ChainParametersVersionFor pv))
    BsoGetNextUpdateSequenceNumber :: MockUpdatableBlockState -> UpdateType -> BlockStateOperationsAction pv UpdateSequenceNumber
    BsoEnqueueUpdate :: MockUpdatableBlockState -> TransactionTime -> UpdateValue (ChainParametersVersionFor pv) -> BlockStateOperationsAction pv MockUpdatableBlockState
    BsoOverwriteElectionDifficulty :: MockUpdatableBlockState -> ElectionDifficulty -> BlockStateOperationsAction pv MockUpdatableBlockState
    BsoClearProtocolUpdate :: MockUpdatableBlockState -> BlockStateOperationsAction pv MockUpdatableBlockState
    BsoAddReleaseSchedule :: MockUpdatableBlockState -> [(AccountAddress, Timestamp)] -> BlockStateOperationsAction pv MockUpdatableBlockState
    BsoGetEnergyRate :: MockUpdatableBlockState -> BlockStateOperationsAction pv EnergyRate
    BsoGetChainParameters :: MockUpdatableBlockState -> BlockStateOperationsAction pv (ChainParameters pv)
    BsoGetEpochBlocksBaked :: MockUpdatableBlockState -> BlockStateOperationsAction pv (Word64, [(BakerId, Word64)])
    BsoNotifyBlockBaked :: MockUpdatableBlockState -> BakerId -> BlockStateOperationsAction pv MockUpdatableBlockState
    BsoClearEpochBlocksBaked :: (AccountVersionFor pv ~ 'AccountV0) => MockUpdatableBlockState -> BlockStateOperationsAction pv MockUpdatableBlockState
    BsoSetNextCapitalDistribution :: (AccountVersionFor pv ~ 'AccountV1) => MockUpdatableBlockState -> CapitalDistribution -> BlockStateOperationsAction pv MockUpdatableBlockState
    BsoRotateCurrentCapitalDistribution :: (AccountVersionFor pv ~ 'AccountV1) => MockUpdatableBlockState -> BlockStateOperationsAction pv MockUpdatableBlockState
    BsoGetBankStatus :: MockUpdatableBlockState -> BlockStateOperationsAction pv BankStatus
    BsoSetRewardAccounts :: MockUpdatableBlockState -> RewardAccounts -> BlockStateOperationsAction pv MockUpdatableBlockState

deriving instance Eq (BlockStateOperationsAction pv a)
deriving instance Show (BlockStateOperationsAction pv a)

generateAct ''BlockStateOperationsAction

data Action pv a where
    AO :: AccountOperationsAction pv a -> Action pv a
    CO :: ContractStateOperationsAction a -> Action pv a
    BSQ :: BlockStateQueryAction pv a -> Action pv a
    BSO :: BlockStateOperationsAction pv a -> Action pv a

deriving instance Eq (Action pv a)
deriving instance Show (Action pv a)

instance Act (Action pv) where
    eqAct (AO x) (AO y) = eqAct x y
    eqAct (BSQ x) (BSQ y) = eqAct x y
    eqAct (BSO x) (BSO y) = eqAct x y
    eqAct _ _ = Nothing
    showRes (AO x) r = showRes x r
    showRes (CO x) _ = case x of
    showRes (BSQ x) r = showRes x r
    showRes (BSO x) r = showRes x r

newtype MockT f m a = MockT {runMockT' :: StateT [WithResult f] m a}
    deriving (Functor, Applicative, Monad, MonadTrans)

type Mock f a = MockT f Identity a

-- |Given the expected trace of a monadic action, run the action, mocking the results of the monadic
-- calls, checking that the behaviour matches the expected trace.
runMockT :: (Act f, Monad m) => [WithResult f] -> MockT f m a -> m a
runMockT actions (MockT mock) =
    runStateT mock actions >>= \case
        (res, []) -> return res
        (_, a : as) -> error $ "Not all actions were consumed: " ++ show a ++ " (and " ++ show (length as) ++ " more)"

-- |A specialisation of 'runMockT' to the 'Identity' underlying monad.
runMock :: (Act f) => [WithResult f] -> Mock f a -> a
runMock actions = runIdentity . runMockT actions

-- |Mock up an action by checking that the arguments are as expected and returning the result
-- provided by the trace.
mockAction :: (Act f, Monad m) => f a -> MockT f m a
mockAction act =
    MockT $
        get >>= \case
            [] -> error $ "No actions remain, but attempted: " ++ show act
            (a :-> r) : racts -> case eqAct act a of
                Nothing -> error $ "Expected action: " ++ show a ++ "\nBut saw: " ++ show act
                Just Refl -> do
                    put racts
                    return r

liftMockAction2 :: (Act f, Monad m) => (aa -> f a) -> (x -> y -> aa) -> x -> y -> MockT f m a
liftMockAction2 l mka x y = mockAction $ l $ mka x y

liftMockAction3 :: (Act f, Monad m) => (aa -> f a) -> (x -> y -> z -> aa) -> x -> y -> z -> MockT f m a
liftMockAction3 l mka x y z = mockAction $ l $ mka x y z

liftMockAction4 :: (Act f, Monad m) => (aa -> f a) -> (w -> x -> y -> z -> aa) -> w -> x -> y -> z -> MockT f m a
liftMockAction4 l mka w x y z = mockAction $ l $ mka w x y z

instance IsProtocolVersion pv => MonadProtocolVersion (MockT (Action pv) m) where
    type MPV (MockT (Action pv) m) = pv

instance BlockStateTypes (MockT (Action pv) m) where
    type BlockState (MockT (Action pv) m) = MockBlockState
    type UpdatableBlockState (MockT (Action pv) m) = MockUpdatableBlockState
    type Account (MockT (Action pv) m) = MockAccount
    type ContractState (MockT (Action pv) m) = MockContractState
    type BakerInfoRef (MockT (Action pv) m) = MockBakerInfoRef

mockOperations
    [d|instance (Monad m) => AccountOperations (MockT (Action pv) m)|]
    ''AccountOperationsAction
    [|mockAction . AO|]

mockOperations
    [d|instance (Monad m) => ContractStateOperations (MockT (Action pv) m) where
         thawContractState = error "Unsupported operation."
         getV1StateContext = error "Unsupported operation."
         stateSizeV0 = error "Unsupported operation."
         contractStateToByteString = error "Unsupported operation."
         |]
    ''ContractStateOperationsAction
    [|mockAction . CO|]

mockOperations
    [d|instance (Monad m) => BlockStateQuery (MockT (Action pv) m)|]
    ''BlockStateQueryAction
    [|mockAction . BSQ|]

mockOperations
    [d|instance (Monad m) => BlockStateOperations (MockT (Action pv) m) where
        bsoPutNewInstance = error "Unsupported operation"
        bsoModifyInstance = error "Unsupported operation"
        bsoPutNewModule = error "Unsupported operation"
        bsoProcessPendingChanges = error "Unsupported operation"
        |]
    ''BlockStateOperationsAction
    [|mockAction . BSO|]
