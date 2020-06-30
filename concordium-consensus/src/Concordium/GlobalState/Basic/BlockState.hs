{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
module Concordium.GlobalState.Basic.BlockState where

import Lens.Micro.Platform
import Concordium.Utils
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import qualified Data.List as List
import Data.Maybe

import GHC.Generics (Generic)

import Concordium.ID.Types(cdvRegId)
import Concordium.Types
import qualified Concordium.GlobalState.Types as GT
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.AccountTransactionIndex
import Concordium.GlobalState.Basic.BlockState.Bakers
import qualified Concordium.GlobalState.BlockState as BS
import qualified Concordium.GlobalState.Modules as Modules
import qualified Concordium.GlobalState.Basic.BlockState.Account as Account
import qualified Concordium.GlobalState.Basic.BlockState.Instances as Instances
import qualified Concordium.GlobalState.Rewards as Rewards
import qualified Concordium.GlobalState.IdentityProviders as IPS
import qualified Concordium.Types.Transactions as Transactions
import Concordium.GlobalState.SeedState

import qualified Acorn.Utils.Init as Acorn

data BasicBirkParameters = BasicBirkParameters {
    _birkElectionDifficulty :: ElectionDifficulty,
    -- |The current stake of bakers. All updates should be to this state.
    _birkCurrentBakers :: !Bakers,
    -- |The state of bakers at the end of the previous epoch,
    -- will be used as lottery bakers in next epoch.
    _birkPrevEpochBakers :: !Bakers,
    -- |The state of the bakers fixed before previous epoch,
    -- the lottery power and reward account is used in leader election.
    _birkLotteryBakers :: !Bakers,
    _birkSeedState :: !SeedState
} deriving (Eq, Generic, Show)

data BlockState = BlockState {
    _blockAccounts :: !Account.Accounts,
    _blockInstances :: !Instances.Instances,
    _blockModules :: !Modules.Modules,
    _blockBank :: !Rewards.BankStatus,
    _blockIdentityProviders :: !IPS.IdentityProviders,
    _blockBirkParameters :: !BasicBirkParameters,
    _blockCryptographicParameters :: !CryptographicParameters,
    _blockTransactionOutcomes :: !Transactions.TransactionOutcomes
} deriving (Show)

makeLenses ''BasicBirkParameters
makeLenses ''BlockState

-- |Mostly empty block state, apart from using 'Rewards.genesisBankStatus' which
-- has hard-coded initial values for amount of gtu in existence.
emptyBlockState :: BasicBirkParameters -> CryptographicParameters -> BlockState
emptyBlockState _blockBirkParameters _blockCryptographicParameters = BlockState {
  _blockAccounts = Account.emptyAccounts
  , _blockInstances = Instances.emptyInstances
  , _blockModules = Modules.emptyModules
  , _blockBank = Rewards.emptyBankStatus
  , _blockIdentityProviders = IPS.emptyIdentityProviders
  , _blockTransactionOutcomes = Transactions.emptyTransactionOutcomes
  ,..
  }


newtype PureBlockStateMonad m a = PureBlockStateMonad {runPureBlockStateMonad :: m a}
    deriving (Functor, Applicative, Monad)

instance GT.BlockStateTypes (PureBlockStateMonad m) where
    type BlockState (PureBlockStateMonad m) = BlockState
    type UpdatableBlockState (PureBlockStateMonad m) = BlockState
    type BirkParameters (PureBlockStateMonad m) = BasicBirkParameters
    type Bakers (PureBlockStateMonad m) = Bakers

instance ATITypes (PureBlockStateMonad m) where
  type ATIStorage (PureBlockStateMonad m) = ()

instance Monad m => PerAccountDBOperations (PureBlockStateMonad m) where
  -- default implementation

instance Monad m => BS.BlockStateQuery (PureBlockStateMonad m) where
    {-# INLINE getModule #-}
    getModule bs mref =
        return $ bs ^. blockModules . to (Modules.getModule mref)

    {-# INLINE getContractInstance #-}
    getContractInstance bs caddr = return (Instances.getInstance caddr (bs ^. blockInstances))

    {-# INLINE getAccount #-}
    getAccount bs aaddr =
      return $ bs ^? blockAccounts . ix aaddr

    {-# INLINE getModuleList #-}
    getModuleList bs = return $ bs ^. blockModules . to Modules.moduleList

    {-# INLINE getContractInstanceList #-}
    getContractInstanceList bs = return (bs ^.. blockInstances . Instances.foldInstances)

    {-# INLINE getAccountList #-}
    getAccountList bs =
      return $ Map.keys (Account.accountMap (bs ^. blockAccounts))

    {-# INLINE getBlockBirkParameters #-}
    getBlockBirkParameters = return . _blockBirkParameters

    {-# INLINE getRewardStatus #-}
    getRewardStatus = return . _blockBank

    {-# INLINE getTransactionOutcome #-}
    getTransactionOutcome bs trh =
        return $ bs ^? blockTransactionOutcomes . ix trh

    {-# INLINE getOutcomes #-}
    getOutcomes bs =
        return $ bs ^. blockTransactionOutcomes . to Transactions.outcomeValues

    {-# INLINE getSpecialOutcomes #-}
    getSpecialOutcomes bs =
        return $ bs ^. blockTransactionOutcomes . Transactions.outcomeSpecial

instance Monad m => BS.BakerQuery (PureBlockStateMonad m) where

  getBakerStake bs bid = return $ bs ^? bakerMap . ix bid . bakerStake

  getBakerFromKey bs k = return $ bs ^. bakersByKey . at' k

  getTotalBakerStake bs = return $ bs ^. bakerTotalStake

  getBakerInfo bs bid = return $ bs ^? bakerMap . ix bid . bakerInfo
  
  getFullBakerInfos = return . _bakerMap        

instance Monad m => BS.BirkParametersOperations (PureBlockStateMonad m) where

    getSeedState bps = return $ _birkSeedState bps

    updateBirkParametersForNewEpoch seedState = return . basicUpdateBirkParametersForNewEpoch seedState

    getElectionDifficulty = return . _birkElectionDifficulty

    getCurrentBakers = return . _birkCurrentBakers

    getLotteryBakers = return . _birkLotteryBakers

    updateSeedState f bps = return $ bps & birkSeedState %~ f

basicUpdateBirkParametersForNewEpoch :: SeedState -> BasicBirkParameters -> BasicBirkParameters
basicUpdateBirkParametersForNewEpoch seedState bps = bps &
    birkSeedState .~ seedState &
    -- use stake distribution saved from the former epoch for leader election
    birkLotteryBakers .~ (bps ^. birkPrevEpochBakers) &
    -- save the stake distribution from the end of the epoch
    birkPrevEpochBakers .~ (bps ^. birkCurrentBakers)

instance Monad m => BS.BlockStateOperations (PureBlockStateMonad m) where

    {-# INLINE bsoGetModule #-}
    bsoGetModule bs mref = return $ bs ^. blockModules . to (Modules.getModule mref)

    {-# INLINE bsoGetInstance #-}
    bsoGetInstance bs caddr = return (Instances.getInstance caddr (bs ^. blockInstances))

    {-# INLINE bsoGetAccount #-}
    bsoGetAccount bs aaddr =
      return $ bs ^? blockAccounts . ix aaddr

    {-# INLINE bsoRegIdExists #-}
    bsoRegIdExists bs regid = return (Account.regIdExists regid (bs ^. blockAccounts))

    {-# INLINE bsoPutNewAccount #-}
    bsoPutNewAccount bs acc = return $
        if Account.exists addr accounts then
          (False, bs)
        else
          (True, bs & blockAccounts .~ Account.putAccount acc accounts & bakerUpdate)
        where
            accounts = bs ^. blockAccounts
            addr = acc ^. accountAddress
            bakerUpdate = blockBirkParameters . birkCurrentBakers %~ addStake (acc ^. accountStakeDelegate) (acc ^. accountAmount)

    bsoPutNewInstance bs mkInstance = return (instanceAddress, bs')
        where
            (inst, instances') = Instances.createInstance mkInstance (bs ^. blockInstances)
            Instances.InstanceParameters{..} = Instances.instanceParameters inst
            bs' = bs
                -- Add the instance
                & blockInstances .~ instances'
                -- Update the owner account's set of instances
                & blockAccounts . ix instanceOwner . accountInstances %~ Set.insert instanceAddress
                -- Delegate the stake as needed
                & maybe (error "Instance has invalid owner")
                    (\owner -> blockBirkParameters . birkCurrentBakers %~ addStake (owner ^. accountStakeDelegate) (Instances.instanceAmount inst))
                    (bs ^? blockAccounts . ix instanceOwner)

    bsoPutNewModule bs mref iface viface source = return $!
        case Modules.putInterfaces mref iface viface source (bs ^. blockModules) of
          Nothing -> (False, bs)
          Just mods' -> (True, bs & blockModules .~ mods')

    bsoTryGetLinkedExpr bs mref n = return $!
        Modules.getLinkedExpr mref n (bs ^. blockModules)

    bsoPutLinkedExpr bs mref n linked = return $!
        bs & blockModules %~ (Modules.putLinkedExpr mref n linked)


    bsoTryGetLinkedContract bs mref n = return $!
        Modules.getLinkedContract mref n (bs ^. blockModules)

    bsoPutLinkedContract bs mref n linked = return $!
        bs & blockModules %~ (Modules.putLinkedContract mref n linked)

    bsoModifyInstance bs caddr delta model = return $!
        bs & blockInstances %~ Instances.updateInstanceAt caddr delta model
        & maybe (error "Instance has invalid owner")
            (\owner -> blockBirkParameters . birkCurrentBakers %~ modifyStake (owner ^. accountStakeDelegate) delta)
            (bs ^? blockAccounts . ix instanceOwner)
        where
            inst = fromMaybe (error "Instance does not exist") $ bs ^? blockInstances . ix caddr
            Instances.InstanceParameters{..} = Instances.instanceParameters inst

    bsoModifyAccount bs accountUpdates = return $!
        -- Update the account
        (case accountUpdates ^. BS.auCredential of
             Nothing -> bs & blockAccounts %~ Account.putAccount updatedAccount
             Just cdi ->
               bs & blockAccounts %~ Account.putAccount updatedAccount
                                   . Account.recordRegId (cdvRegId cdi))
        -- If we change the amount, update the delegate
        & (blockBirkParameters . birkCurrentBakers
                    %~ modifyStake (account ^. accountStakeDelegate)
                                   (accountUpdates ^. BS.auAmount . non 0))
        where
            account = bs ^. blockAccounts . singular (ix (accountUpdates ^. BS.auAddress))
            updatedAccount = BS.updateAccount accountUpdates account

    {-# INLINE bsoNotifyExecutionCost #-}
    bsoNotifyExecutionCost bs amnt =
      return . snd $ bs & blockBank . Rewards.executionCost <%~ (+ amnt)

    bsoNotifyIdentityIssuerCredential bs idk =
      let updatedRewards = HashMap.alter (Just . maybe 1 (+1)) idk (bs ^. blockBank ^. Rewards.identityIssuersRewards) in
      return $! bs & blockBank . Rewards.identityIssuersRewards .~ updatedRewards

    {-# INLINE bsoGetExecutionCost #-}
    bsoGetExecutionCost bs =
      return $ bs ^. blockBank . Rewards.executionCost

    {-# INLINE bsoGetBlockBirkParameters #-}
    bsoGetBlockBirkParameters = return . _blockBirkParameters

    bsoAddBaker bs binfo = return $!
        case createBaker binfo (bs ^. blockBirkParameters . birkCurrentBakers) of
          Right (bid, newBakers) -> (Right bid, bs & blockBirkParameters . birkCurrentBakers .~ newBakers)
          Left err -> (Left err, bs)

    -- NB: The caller must ensure the baker exists. Otherwise this method is incorrect and will raise a runtime error.
    bsoUpdateBaker bs bupdate = return $!
        let bakers = bs ^. blockBirkParameters . birkCurrentBakers
        in case updateBaker bupdate bakers of
             Nothing -> (False, bs)
             Just newBakers -> (True, bs & blockBirkParameters . birkCurrentBakers .~ newBakers)

    bsoRemoveBaker bs bid = return $
        let
            (rv, bakers') = removeBaker bid $ bs ^. blockBirkParameters . birkCurrentBakers
        in (rv, bs & blockBirkParameters . birkCurrentBakers .~ bakers')

    bsoSetInflation bs amnt = return $
        bs & blockBank . Rewards.mintedGTUPerSlot .~ amnt

    -- mint currency in the central bank, and also update the total gtu amount to maintain the invariant
    -- that the total gtu amount is indeed the total gtu amount
    bsoMint bs amount = return $
        let updated = bs & ((blockBank . Rewards.totalGTU) +~ amount) .
                           ((blockBank . Rewards.centralBankGTU) +~ amount)
        in (updated ^. blockBank . Rewards.centralBankGTU, updated)

    bsoDecrementCentralBankGTU bs amount = return $!
        let updated = bs & ((blockBank . Rewards.centralBankGTU) -~ amount)
        in (updated ^. blockBank . Rewards.centralBankGTU, updated)

    bsoDelegateStake bs aaddr target = return $! if targetValid then (True, bs') else (False, bs)
        where
            targetValid = case target of
                Nothing -> True
                Just bid -> isJust $ bs ^. blockBirkParameters . birkCurrentBakers . bakerMap . at' bid
            acct = fromMaybe (error "Invalid account address") $ bs ^? blockAccounts . ix aaddr
            stake = acct ^. accountAmount +
                sum [Instances.instanceAmount inst |
                        Just inst <- Set.toList (acct ^. accountInstances) <&> flip Instances.getInstance (bs ^. blockInstances)]
            bs' = bs & blockBirkParameters . birkCurrentBakers %~ removeStake (acct ^. accountStakeDelegate) stake . addStake target stake
                    & blockAccounts . ix aaddr %~ (accountStakeDelegate .~ target)

    {-# INLINE bsoGetIdentityProvider #-}
    bsoGetIdentityProvider bs ipId =
      return $! bs ^? blockIdentityProviders . to IPS.idProviders . ix ipId

    {-# INLINE bsoGetCryptoParams #-}
    bsoGetCryptoParams bs =
      return $! bs ^. blockCryptographicParameters

    bsoSetTransactionOutcomes bs l =
      return $! bs & blockTransactionOutcomes .~ Transactions.transactionOutcomesFromList l

    bsoAddSpecialTransactionOutcome bs o =
      return $! bs & blockTransactionOutcomes . Transactions.outcomeSpecial %~ (o:)

    {-# INLINE bsoUpdateBirkParameters #-}
    bsoUpdateBirkParameters bs bps = return $! bs & blockBirkParameters .~ bps

    {-# INLINE bsoSetElectionDifficulty #-}
    bsoSetElectionDifficulty bs d = return $!
      bs & blockBirkParameters . birkElectionDifficulty .~ d

instance Monad m => BS.BlockStateStorage (PureBlockStateMonad m) where
    {-# INLINE thawBlockState #-}
    thawBlockState bs = return $ bs & (blockBank . Rewards.executionCost .~ 0) .
                                      (blockBank . Rewards.identityIssuersRewards .~ HashMap.empty)

    {-# INLINE freezeBlockState #-}
    freezeBlockState = return

    {-# INLINE dropUpdatableBlockState #-}
    dropUpdatableBlockState _ = return ()

    {-# INLINE purgeBlockState #-}
    purgeBlockState _ = return ()

    {-# INLINE archiveBlockState #-}
    archiveBlockState _ = return ()

    {-# INLINE putBlockState #-}
    putBlockState _ = return (return ())

    {-# INLINE getBlockState #-}
    getBlockState = fail "Basic block state cannot be deserialized"


-- |Initial block state.
initialState :: BasicBirkParameters
             -> CryptographicParameters
             -> [Account]
             -> [IPS.IpInfo]
             -> Amount
             -> BlockState
initialState _blockBirkParameters _blockCryptographicParameters genesisAccounts ips mintPerSlot = BlockState{..}
  where
    _blockAccounts = List.foldl' (flip Account.putAccountWithRegIds) Account.emptyAccounts genesisAccounts
    _blockInstances = Instances.emptyInstances
    _blockModules = Modules.fromModuleList (Acorn.moduleList (let (_, _, pm) = Acorn.baseState in pm))
    _blockBank = Rewards.makeGenesisBankStatus initialAmount mintPerSlot
    _blockIdentityProviders = IPS.IdentityProviders (HashMap.fromList (map (\r -> (IPS.ipIdentity r, r)) ips))
    _blockTransactionOutcomes = Transactions.emptyTransactionOutcomes

    -- initial amount in the central bank is the amount on all genesis accounts combined
    initialAmount = List.foldl' (\c acc -> c + acc ^. accountAmount) 0 $ genesisAccounts
