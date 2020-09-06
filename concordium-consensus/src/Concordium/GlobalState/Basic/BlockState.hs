{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
module Concordium.GlobalState.Basic.BlockState where

import Lens.Micro.Platform
import Concordium.Utils
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import qualified Data.List as List
import Data.Maybe
import Control.Monad

import GHC.Generics (Generic)

import Concordium.Types
import qualified Concordium.GlobalState.Types as GT
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.AccountTransactionIndex
import Concordium.GlobalState.Basic.BlockState.Bakers
import qualified Concordium.GlobalState.BlockState as BS
import qualified Concordium.GlobalState.Modules as Modules
import Concordium.GlobalState.Basic.BlockState.Account
import qualified Concordium.GlobalState.Basic.BlockState.Accounts as Accounts
import qualified Concordium.GlobalState.Basic.BlockState.Instances as Instances
import qualified Concordium.GlobalState.Rewards as Rewards
import qualified Concordium.GlobalState.IdentityProviders as IPS
import qualified Concordium.GlobalState.AnonymityRevokers as ARS
import qualified Concordium.Types.Transactions as Transactions
import Concordium.GlobalState.SeedState
import Concordium.ID.Types (cdvRegId)

import qualified Concordium.Crypto.SHA256 as H
import qualified Concordium.GlobalState.Basic.BlockState.LFMBTree as L
import Concordium.Types.HashableTo
import Data.Serialize

data BasicBirkParameters = BasicBirkParameters {
    _birkElectionDifficulty :: ElectionDifficulty,
    -- |The current stake of bakers. All updates should be to this state.
    _birkCurrentBakers :: !Bakers,
    _birkCurrentBakersHash :: !(Maybe H.Hash),
    -- |The state of bakers at the end of the previous epoch,
    -- will be used as lottery bakers in next epoch.
    _birkPrevEpochBakers :: !(Hashed Bakers),
    -- |The state of the bakers fixed before previous epoch,
    -- the lottery power and reward account is used in leader election.
    _birkLotteryBakers :: !(Hashed Bakers),
    _birkSeedState :: !SeedState
} deriving (Eq, Generic, Show)

instance HashableTo H.Hash BasicBirkParameters where
    getHash BasicBirkParameters {..} = H.hashOfHashes bpH0 bpH2
      where
        bpH0 = H.hash $ "ElectDiff" <> encode _birkElectionDifficulty <> "SeedState" <> encode _birkSeedState
        bpH1 = H.hashOfHashes (getHash _birkPrevEpochBakers) (getHash _birkLotteryBakers)
        bpH2 = H.hashOfHashes (fromMaybe (getHash _birkCurrentBakers) _birkCurrentBakersHash) bpH1

-- | Create a BasicBirkParameters value from the components
makeBirkParameters ::
  ElectionDifficulty
  -> Bakers -- ^ Set of current bakers
  -> Bakers -- ^ Set of bakers on the previous epoch
  -> Bakers -- ^ Set of lottery bakers
  -> SeedState
  -> BasicBirkParameters
makeBirkParameters _birkElectionDifficulty _birkCurrentBakers prevEpochBakers lotteryBakers _birkSeedState = BasicBirkParameters {_birkCurrentBakersHash = Just (getHash _birkCurrentBakers), ..}
  where
    _birkPrevEpochBakers = makeHashed prevEpochBakers
    _birkLotteryBakers = makeHashed lotteryBakers

-- | Subhashes and the top level hash for the BlockState
data BlockStateHashes
  = BlockStateHashes
      {
        hashOfBirkParamsAndCryptoParams :: H.Hash,
        hashOfIPsAndARs :: H.Hash,
        hashOfModulesAndBank :: H.Hash,
        hashOfAccountsAndInstances :: H.Hash,
        hashOfBirkCryptoIPsARs :: H.Hash,
        hashOfModulesBankAccountsIntances :: H.Hash,
        -- | The top level hash of the BlockState. Hash of hashOfIPsAndARs0 and hashOfIPsAndARs1
        blockStateHash :: StateHash
      }
  deriving (Show, Eq)

data BlockState = BlockState {
    _blockAccounts :: !Accounts.Accounts,
    _blockInstances :: !Instances.Instances,
    _blockModules :: !Modules.Modules,
    _blockBank :: !(Hashed Rewards.BankStatus),
    _blockIdentityProviders :: !(Hashed IPS.IdentityProviders),
    _blockAnonymityRevokers :: !(Hashed ARS.AnonymityRevokers),
    _blockBirkParameters :: !BasicBirkParameters,
    _blockCryptographicParameters :: !(Hashed CryptographicParameters),
    _blockTransactionOutcomes :: !Transactions.TransactionOutcomes,
    _blockHashes :: !BlockStateHashes
} deriving (Show)

makeLenses ''BasicBirkParameters
makeLenses ''BlockState

-- |Mostly empty block state, apart from using 'Rewards.genesisBankStatus' which
-- has hard-coded initial values for amount of gtu in existence.
emptyBlockState :: BasicBirkParameters -> CryptographicParameters -> BlockState
emptyBlockState _blockBirkParameters cryptographicParameters = BlockState
          { _blockTransactionOutcomes = Transactions.emptyTransactionOutcomes,
            _blockHashes = makeBlockStateHashes'
                             _blockBirkParameters
                             _blockCryptographicParameters
                             _blockIdentityProviders
                             _blockAnonymityRevokers
                             _blockModules
                             _blockBank
                             _blockAccounts
                             _blockInstances,
            ..
          }
    where
      _blockCryptographicParameters = makeHashed cryptographicParameters
      _blockAccounts = Accounts.emptyAccounts
      _blockInstances = Instances.emptyInstances
      _blockModules = Modules.emptyModules
      _blockBank = makeHashed Rewards.emptyBankStatus
      _blockIdentityProviders = makeHashed IPS.emptyIdentityProviders
      _blockAnonymityRevokers = makeHashed ARS.emptyAnonymityRevokers

-- | Given a block, calculates the top level hash, and subhashes
makeBlockStateHashes :: BlockState -> BlockStateHashes
makeBlockStateHashes BlockState {..} = makeBlockStateHashes' _blockBirkParameters _blockCryptographicParameters _blockIdentityProviders _blockAnonymityRevokers _blockModules _blockBank _blockAccounts _blockInstances

makeBlockStateHashes' ::
  BasicBirkParameters
  -> Hashed CryptographicParameters
  -> Hashed IPS.IdentityProviders
  -> Hashed ARS.AnonymityRevokers
  -> Modules.Modules
  -> Hashed Rewards.BankStatus
  -> Accounts.Accounts
  -> Instances.Instances
  -> BlockStateHashes
makeBlockStateHashes' birkParameters cryptoParams ips ars mods bank accs instances = BlockStateHashes {..}
    where
      hashOfBirkParamsAndCryptoParams = H.hashOfHashes (getHash birkParameters) (getHash cryptoParams)
      hashOfIPsAndARs = H.hashOfHashes (getHash ips) (getHash ars)
      hashOfModulesAndBank = H.hashOfHashes (getHash mods) (getHash bank)
      hashOfAccountsAndInstances = H.hashOfHashes (getHash accs) (getHash instances)
      hashOfBirkCryptoIPsARs = H.hashOfHashes hashOfBirkParamsAndCryptoParams hashOfIPsAndARs
      hashOfModulesBankAccountsIntances = H.hashOfHashes hashOfModulesAndBank hashOfAccountsAndInstances
      blockStateHash = StateHashV0 (H.hashOfHashes hashOfBirkCryptoIPsARs hashOfModulesBankAccountsIntances)

instance HashableTo StateHash BlockState where
    getHash BlockState {..} = blockStateHash _blockHashes

newtype PureBlockStateMonad m a = PureBlockStateMonad {runPureBlockStateMonad :: m a}
    deriving (Functor, Applicative, Monad)

type instance GT.BlockStatePointer BlockState = ()

instance GT.BlockStateTypes (PureBlockStateMonad m) where
    type BlockState (PureBlockStateMonad m) = BlockState
    type UpdatableBlockState (PureBlockStateMonad m) = BlockState
    type BirkParameters (PureBlockStateMonad m) = BasicBirkParameters
    type Bakers (PureBlockStateMonad m) = Bakers
    type Account (PureBlockStateMonad m) = Account

instance ATITypes (PureBlockStateMonad m) where
  type ATIStorage (PureBlockStateMonad m) = ()

instance Monad m => PerAccountDBOperations (PureBlockStateMonad m)
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
      return $ Map.keys (Accounts.accountMap (bs ^. blockAccounts))

    {-# INLINE getBlockBirkParameters #-}
    getBlockBirkParameters = return . _blockBirkParameters

    {-# INLINE getRewardStatus #-}
    getRewardStatus = return . _unhashed . _blockBank

    {-# INLINE getTransactionOutcome #-}
    getTransactionOutcome bs trh =
        return $ bs ^? blockTransactionOutcomes . ix trh

    {-# INLINE getTransactionOutcomesHash #-}
    getTransactionOutcomesHash bs = return (getHash $ _blockTransactionOutcomes $ bs)
    
    {-# INLINE getStateHash #-}
    getStateHash bs = return (blockStateHash $ _blockHashes $ bs)

    {-# INLINE getOutcomes #-}
    getOutcomes bs =
        return $ bs ^. blockTransactionOutcomes . to Transactions.outcomeValues

    {-# INLINE getSpecialOutcomes #-}
    getSpecialOutcomes bs =
        return $ bs ^. blockTransactionOutcomes . Transactions.outcomeSpecial

    {-# INLINE getAllIdentityProviders #-}
    getAllIdentityProviders bs =
      return $! bs ^. blockIdentityProviders . unhashed . to (Map.elems . IPS.idProviders)

    {-# INLINE getAllAnonymityRevokers #-}
    getAllAnonymityRevokers bs = return $! bs ^. blockAnonymityRevokers . unhashed . to (Map.elems . ARS.arRevokers)

instance Monad m => BS.AccountOperations (PureBlockStateMonad m) where

  getAccountAddress acc = return $ acc ^. accountAddress

  getAccountAmount acc = return $ acc ^. accountAmount

  getAccountNonce acc = return $ acc ^. accountNonce

  getAccountCredentials acc = return $ acc ^. accountCredentials

  getAccountMaxCredentialValidTo acc = return $ acc ^. accountMaxCredentialValidTo

  getAccountVerificationKeys acc = return $ acc ^. accountVerificationKeys

  getAccountEncryptedAmount acc = return $ acc ^. accountEncryptedAmount

  getAccountEncryptionKey acc = return $ acc ^. accountEncryptionKey

  getAccountStakeDelegate acc = return $ acc ^. accountStakeDelegate

  getAccountInstances acc = return $ acc ^. accountInstances

  createNewAccount gc keys addr regId = return $ newAccount gc keys addr regId

  updateAccountAmount acc amnt = return $ acc & accountAmount .~ amnt

instance Monad m => BS.BakerQuery (PureBlockStateMonad m) where

  getBakerStake bs bid = return $ bs ^? bakerMap . L.ix bid . traversed . bakerStake

  getBakerFromKey bs k = return $ bs ^. bakersByKey . at' k

  getTotalBakerStake bs = return $ bs ^. bakerTotalStake

  getBakerInfo bs bid = return $ bs ^? bakerMap . L.ix bid . traversed . bakerInfo

  getFullBakerInfos bks = return $ Map.fromAscList ([(i, v) | (i, Just v) <- L.toAscPairList $ _bakerMap bks])

instance Monad m => BS.BirkParametersOperations (PureBlockStateMonad m) where

    getSeedState bps = return $ _birkSeedState bps

    updateBirkParametersForNewEpoch seedState = return . basicUpdateBirkParametersForNewEpoch seedState

    getElectionDifficulty = return . _birkElectionDifficulty

    getCurrentBakers = return . _birkCurrentBakers

    getLotteryBakers = return . _unhashed . _birkLotteryBakers

    updateSeedState f bps = return $ bps & birkSeedState %~ f

basicUpdateBirkParametersForNewEpoch :: SeedState -> BasicBirkParameters -> BasicBirkParameters
basicUpdateBirkParametersForNewEpoch seedState bps = bps &
    birkSeedState .~ seedState &
    -- use stake distribution saved from the former epoch for leader election
    birkLotteryBakers .~ (bps ^. birkPrevEpochBakers) &
    -- save the stake distribution from the end of the epoch
    birkPrevEpochBakers .~ makeHashed (bps ^. birkCurrentBakers)

instance Monad m => BS.BlockStateOperations (PureBlockStateMonad m) where

    {-# INLINE bsoGetModule #-}
    bsoGetModule bs mref = return $ bs ^. blockModules . to (fmap BS.moduleInterface . Modules.getModule mref)

    {-# INLINE bsoGetInstance #-}
    bsoGetInstance bs caddr = return (Instances.getInstance caddr (bs ^. blockInstances))

    {-# INLINE bsoGetAccount #-}
    bsoGetAccount bs aaddr =
      return $ bs ^? blockAccounts . ix aaddr

    {-# INLINE bsoRegIdExists #-}
    bsoRegIdExists bs regid = return (Accounts.regIdExists regid (bs ^. blockAccounts))

    {-# INLINE bsoPutNewAccount #-}
    bsoPutNewAccount bs acc = return $
        if Accounts.exists addr accounts then
          (False, bs)
        else
          (True, bs & blockAccounts .~ Accounts.putAccount acc (foldr Accounts.recordRegId accounts (cdvRegId <$> acc ^. accountCredentials))
                    & bakerUpdate)
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

    bsoPutNewModule bs iface = return $!
        case Modules.putInterfaces iface (bs ^. blockModules) of
          Nothing -> (False, bs)
          Just mods' -> (True, bs & blockModules .~ mods')

    bsoModifyInstance bs caddr delta model = return $!
        bs & blockInstances %~ Instances.updateInstanceAt caddr delta model
        & maybe (error "Instance has invalid owner")
            (\owner -> blockBirkParameters . birkCurrentBakers %~ modifyStake (owner ^. accountStakeDelegate) delta)
            (bs ^? blockAccounts . ix instanceOwner)
        where
            inst = fromMaybe (error "Instance does not exist") $ bs ^? blockInstances . ix caddr
            Instances.InstanceParameters {..} = Instances.instanceParameters inst

    bsoModifyAccount bs accountUpdates = return $!
        -- Update the account
        (case accountUpdates ^. auCredential of
             Nothing -> bs & blockAccounts %~ Accounts.putAccount updatedAccount
             Just cdi ->
               bs & blockAccounts %~ Accounts.putAccount updatedAccount
                                   . Accounts.recordRegId (cdvRegId cdi))
        -- If we change the amount, update the delegate
        & (blockBirkParameters . birkCurrentBakers
                    %~ modifyStake (account ^. accountStakeDelegate)
                                   (accountUpdates ^. auAmount . non 0))
        where
            account = bs ^. blockAccounts . singular (ix (accountUpdates ^. auAddress))
            updatedAccount = Accounts.updateAccount accountUpdates account

    {-# INLINE bsoNotifyExecutionCost #-}
    bsoNotifyExecutionCost bs amnt =
      return $! bs & blockBank . unhashed . Rewards.executionCost %~ (+ amnt)

    {-# INLINE bsoNotifyEncryptedBalanceChange #-}
    bsoNotifyEncryptedBalanceChange bs amntDiff =
      return $! bs & blockBank . unhashed . Rewards.totalEncryptedGTU %~ (applyAmountDelta amntDiff)

    bsoNotifyIdentityIssuerCredential bs idk =
      let updatedRewards = HashMap.alter (Just . maybe 1 (+ 1)) idk (bs ^. blockBank . unhashed . Rewards.identityIssuersRewards) in
      return $! bs & blockBank . unhashed . Rewards.identityIssuersRewards .~ updatedRewards

    {-# INLINE bsoGetExecutionCost #-}
    bsoGetExecutionCost bs =
      return $ bs ^. blockBank . unhashed . Rewards.executionCost

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
        bs & blockBank . unhashed . Rewards.mintedGTUPerSlot .~ amnt

    -- mint currency in the central bank, and also update the total gtu amount to maintain the invariant
    -- that the total gtu amount is indeed the total gtu amount
    bsoMint bs amount = return $
        let updated = bs & ((blockBank . unhashed . Rewards.totalGTU) +~ amount) .
                           ((blockBank . unhashed . Rewards.centralBankGTU) +~ amount)
        in (updated ^. blockBank . unhashed . Rewards.centralBankGTU, updated)

    bsoDecrementCentralBankGTU bs amount = return $!
        let updated = bs & ((blockBank . unhashed . Rewards.centralBankGTU) -~ amount)
        in (updated ^. blockBank . unhashed . Rewards.centralBankGTU, updated)

    bsoDelegateStake bs aaddr target = return $! if targetValid then (True, bs') else (False, bs)
        where
            targetValid = case target of
                Nothing -> True
                Just bid -> isJust (bs ^? blockBirkParameters . birkCurrentBakers . bakerMap . L.ix bid)
            acct = fromMaybe (error "Invalid account address") $ bs ^? blockAccounts . ix aaddr
            stake = acct ^. accountAmount +
                sum [Instances.instanceAmount inst |
                        Just inst <- Set.toList (acct ^. accountInstances) <&> flip Instances.getInstance (bs ^. blockInstances)]
            bs' = bs & blockBirkParameters . birkCurrentBakers %~ removeStake (acct ^. accountStakeDelegate) stake . addStake target stake
                     & blockAccounts . ix aaddr %~ (accountStakeDelegate .~ target)

    {-# INLINE bsoGetIdentityProvider #-}
    bsoGetIdentityProvider bs ipId =
      return $! bs ^? blockIdentityProviders . unhashed . to IPS.idProviders . ix ipId

    {-# INLINE bsoGetAnonymityRevokers #-}
    bsoGetAnonymityRevokers bs arIds = return $!
      let ars = bs ^. blockAnonymityRevokers . unhashed . to ARS.arRevokers
      in forM arIds (flip Map.lookup ars)

    {-# INLINE bsoGetCryptoParams #-}
    bsoGetCryptoParams bs =
      return $! bs ^. blockCryptographicParameters . unhashed

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
    thawBlockState bs = return $ bs & (blockBank . unhashed . Rewards.executionCost .~ 0) .
                                      (blockBank . unhashed . Rewards.identityIssuersRewards .~ HashMap.empty)

    {-# INLINE freezeBlockState #-}
    freezeBlockState bs = do
      let bs' = bs & ((blockBirkParameters . birkCurrentBakersHash) ?~ getHash (bs ^. blockBirkParameters . birkCurrentBakers))
          bs'' = bs' & blockHashes .~ makeBlockStateHashes bs'
      return bs''

    {-# INLINE dropUpdatableBlockState #-}
    dropUpdatableBlockState _ = return ()

    {-# INLINE purgeBlockState #-}
    purgeBlockState _ = return ()

    {-# INLINE archiveBlockState #-}
    archiveBlockState _ = return ()

    {-# INLINE saveBlockState #-}
    saveBlockState _ = return ()

    {-# INLINE loadBlockState #-}
    loadBlockState _ = error "Cannot load memory-based block state"

-- |Initial block state.
initialState :: BasicBirkParameters
             -> CryptographicParameters
             -> [Account]
             -> IPS.IdentityProviders
             -> ARS.AnonymityRevokers
             -> Amount
             -> BlockState
initialState _blockBirkParameters cryptoParams genesisAccounts ips anonymityRevokers mintPerSlot = BlockState {..}
  where
    _blockCryptographicParameters = makeHashed cryptoParams
    _blockAccounts = List.foldl' (flip Accounts.putAccountWithRegIds) Accounts.emptyAccounts genesisAccounts
    _blockInstances = Instances.emptyInstances
    _blockModules = Modules.emptyModules
    -- initial amount in the central bank is the amount on all genesis accounts combined
    initialAmount = List.foldl' (\c acc -> c + acc ^. accountAmount) 0 $ genesisAccounts
    _blockBank = makeHashed $ Rewards.makeGenesisBankStatus initialAmount mintPerSlot
    _blockIdentityProviders = makeHashed ips
    _blockAnonymityRevokers = makeHashed anonymityRevokers
    _blockTransactionOutcomes = Transactions.emptyTransactionOutcomes
    _blockHashes = makeBlockStateHashes' _blockBirkParameters _blockCryptographicParameters _blockIdentityProviders _blockAnonymityRevokers _blockModules _blockBank _blockAccounts _blockInstances
