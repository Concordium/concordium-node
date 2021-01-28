{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Concordium.GlobalState.Basic.BlockState where

import Data.Map (Map)
import Lens.Micro.Platform
import Data.Maybe
import Data.Semigroup
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Vector as Vec
import Control.Monad
import Data.Foldable
import Data.Serialize
import qualified Data.Sequence as Seq

import Concordium.Types
import Concordium.Types.Updates
import qualified Concordium.GlobalState.Types as GT
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.AccountTransactionIndex
import Concordium.GlobalState.Basic.BlockState.Bakers
import qualified Concordium.GlobalState.BlockState as BS
import Concordium.GlobalState.Basic.BlockState.Account
import qualified Concordium.GlobalState.Basic.BlockState.Accounts as Accounts
import qualified Concordium.GlobalState.Basic.BlockState.Modules as Modules
import qualified Concordium.GlobalState.Basic.BlockState.Instances as Instances
import qualified Concordium.GlobalState.Rewards as Rewards
import qualified Concordium.Types.IdentityProviders as IPS
import qualified Concordium.Types.AnonymityRevokers as ARS
import Concordium.GlobalState.Basic.BlockState.Updates
import qualified Concordium.Types.Transactions as Transactions
import Concordium.GlobalState.Basic.BlockState.AccountReleaseSchedule
import Concordium.Types.SeedState
import Concordium.ID.Types (regId)
import qualified Concordium.Crypto.SHA256 as H
import Concordium.Types.HashableTo

data BasicBirkParameters = BasicBirkParameters {
    -- |The currently-registered bakers.
    _birkActiveBakers :: !ActiveBakers,
    -- |The bakers that will be used for the next epoch.
    _birkNextEpochBakers :: !(Hashed EpochBakers),
    -- |The bakers for the current epoch.
    _birkCurrentEpochBakers :: !(Hashed EpochBakers),
    -- |The seed state used to derive the leadership election nonce.
    _birkSeedState :: !SeedState
} deriving (Eq, Show)

-- |The hash of the birk parameters derives from the seed state
-- and the bakers for the current and next epochs.  The active
-- bakers are not included, because they derive from the accounts.
instance HashableTo H.Hash BasicBirkParameters where
    getHash BasicBirkParameters {..} = H.hashOfHashes bpH0 bpH1
      where
        bpH0 = H.hash $ "SeedState" <> encode _birkSeedState
        bpH1 = H.hashOfHashes (getHash _birkNextEpochBakers) (getHash _birkCurrentEpochBakers)

-- | Create a BasicBirkParameters value from the components
makeBirkParameters ::
  ActiveBakers -- ^ Set of currently-registered bakers
  -> EpochBakers -- ^ Set of bakers for the next epoch
  -> EpochBakers -- ^ Set of bakers for the current epoch
  -> SeedState
  -> BasicBirkParameters
makeBirkParameters _birkActiveBakers nextEpochBakers currentEpochBakers _birkSeedState = BasicBirkParameters {..}
  where
    _birkNextEpochBakers = makeHashed nextEpochBakers
    _birkCurrentEpochBakers = makeHashed currentEpochBakers

initialBirkParameters ::
  [Account]
  -- ^The accounts at genesis, in order
  -> SeedState
  -- ^The seed state
  -> BasicBirkParameters
initialBirkParameters accounts = makeBirkParameters activeBkrs eBkrs eBkrs
  where
    abi AccountBaker{..} = (_accountBakerInfo, _stakedAmount)
    bkr acct = abi <$> acct ^. accountBaker
    bkrs = catMaybes $ bkr <$> accounts
    activeBkrs = ActiveBakers {
      _activeBakers = Set.fromList (_bakerIdentity . fst <$> bkrs),
      _aggregationKeys = Set.fromList (_bakerAggregationVerifyKey . fst <$> bkrs)
    }
    stakes = Vec.fromList (snd <$> bkrs)
    eBkrs = EpochBakers {
      _bakerInfos = Vec.fromList (fst <$> bkrs),
      _bakerStakes = stakes,
      _bakerTotalStake = sum stakes
    }

-- |List of bakers of blocks baked in the current epoch, used for 
-- rewarding the bakers at the end of the epoch.  This maintains
-- a running hash of the list as 'hebHash'.
--
-- > hebHash == foldr Rewards.epochBlockHash Rewards.emptyEpochBlocksHash hebBlocks
data HashedEpochBlocks = HashedEpochBlocks {
    -- |Bakers of blocks baked in the current epoch, most recent first.
    hebBlocks :: ![BakerId],
    -- |Hash of the list.
    hebHash :: !Rewards.EpochBlocksHash
  } deriving (Eq, Show)
instance HashableTo Rewards.EpochBlocksHash HashedEpochBlocks where
    getHash = hebHash

-- |The 'HashedEpochBlocks' with no blocks.
emptyHashedEpochBlocks :: HashedEpochBlocks
emptyHashedEpochBlocks = HashedEpochBlocks [] Rewards.emptyEpochBlocksHash

-- |Extend a 'HashedEpochBlocks' with an additional baker.
consEpochBlock :: BakerId -> HashedEpochBlocks -> HashedEpochBlocks
consEpochBlock bid heb = HashedEpochBlocks {
    hebBlocks = bid : hebBlocks heb,
    hebHash = Rewards.epochBlockHash bid (hebHash heb)
  }

data BlockState = BlockState {
    _blockAccounts :: !Accounts.Accounts,
    _blockInstances :: !Instances.Instances,
    _blockModules :: !Modules.Modules,
    _blockBank :: !(Hashed Rewards.BankStatus),
    _blockIdentityProviders :: !(Hashed IPS.IdentityProviders),
    _blockAnonymityRevokers :: !(Hashed ARS.AnonymityRevokers),
    _blockBirkParameters :: !BasicBirkParameters,
    _blockCryptographicParameters :: !(Hashed CryptographicParameters),
    _blockUpdates :: !Updates,
    _blockReleaseSchedule :: !(Map AccountAddress Timestamp), -- ^Contains an entry for each account that has pending releases and the first timestamp for said account
    _blockTransactionOutcomes :: !Transactions.TransactionOutcomes,
    _blockEpochBlocksBaked :: !HashedEpochBlocks
} deriving (Show)

data HashedBlockState = HashedBlockState {
    _unhashedBlockState :: !BlockState,
    _blockStateHash :: !StateHash
} deriving (Show)

makeLenses ''BasicBirkParameters
makeClassy ''BlockState
makeLenses ''HashedBlockState

instance HasBlockState HashedBlockState where
    blockState = unhashedBlockState

instance HashableTo StateHash HashedBlockState where
    getHash = _blockStateHash

-- |Mostly empty block state, apart from using 'Rewards.genesisBankStatus' which
-- has hard-coded initial values for amount of gtu in existence.
emptyBlockState :: BasicBirkParameters -> CryptographicParameters -> Authorizations -> ChainParameters -> BlockState
emptyBlockState _blockBirkParameters cryptographicParameters auths chainParams = BlockState
          { _blockTransactionOutcomes = Transactions.emptyTransactionOutcomes,
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
      _blockUpdates = initialUpdates auths chainParams
      _blockReleaseSchedule = Map.empty
      _blockEpochBlocksBaked = emptyHashedEpochBlocks

-- |Convert a 'BlockState' to a 'HashedBlockState' by computing
-- the state hash.
hashBlockState :: BlockState -> HashedBlockState
hashBlockState bs@BlockState{..} = HashedBlockState {
        _unhashedBlockState = bs,
        _blockStateHash = h
      }
    where
        h = BS.makeBlockStateHash BS.BlockStateHashInputs {
              bshBirkParameters = getHash _blockBirkParameters,
              bshCryptographicParameters = getHash _blockCryptographicParameters,
              bshIdentityProviders = getHash _blockIdentityProviders,
              bshAnonymityRevokers = getHash _blockAnonymityRevokers,
              bshModules = getHash _blockModules,
              bshBankStatus = getHash _blockBank,
              bshAccounts = getHash _blockAccounts,
              bshInstances = getHash _blockInstances,
              bshUpdates = getHash _blockUpdates,
              bshEpochBlocks = getHash _blockEpochBlocksBaked
            }

instance HashableTo StateHash BlockState where
    getHash = _blockStateHash . hashBlockState

newtype PureBlockStateMonad m a = PureBlockStateMonad {runPureBlockStateMonad :: m a}
    deriving (Functor, Applicative, Monad)

type instance GT.BlockStatePointer BlockState = ()
type instance GT.BlockStatePointer HashedBlockState = ()

instance GT.BlockStateTypes (PureBlockStateMonad m) where
    type BlockState (PureBlockStateMonad m) = HashedBlockState
    type UpdatableBlockState (PureBlockStateMonad m) = BlockState
    type Account (PureBlockStateMonad m) = Account

instance ATITypes (PureBlockStateMonad m) where
  type ATIStorage (PureBlockStateMonad m) = ()

instance Monad m => PerAccountDBOperations (PureBlockStateMonad m)
  -- default implementation

instance Monad m => BS.BlockStateQuery (PureBlockStateMonad m) where
    {-# INLINE getModule #-}
    getModule bs mref =
        return $ bs ^. blockModules . to (Modules.getSource mref)

    {-# INLINE getContractInstance #-}
    getContractInstance bs caddr = return (Instances.getInstance caddr (bs ^. blockInstances))

    {-# INLINE getAccount #-}
    getAccount bs aaddr =
      return $ bs ^? blockAccounts . ix aaddr

    {-# INLINE getBakerAccount #-}
    getBakerAccount bs (BakerId ai) =
      return $ bs ^? blockAccounts . Accounts.indexedAccount ai

    {-# INLINE getModuleList #-}
    getModuleList bs = return $ bs ^. blockModules . to Modules.moduleRefList

    {-# INLINE getContractInstanceList #-}
    getContractInstanceList bs = return (bs ^.. blockInstances . Instances.foldInstances)

    {-# INLINE getAccountList #-}
    getAccountList bs =
      return $ Map.keys (Accounts.accountMap (bs ^. blockAccounts))

    getSeedState = return . _birkSeedState . _blockBirkParameters . _unhashedBlockState

    getCurrentEpochBakers = return . epochToFullBakers . _unhashed . _birkCurrentEpochBakers . _blockBirkParameters . _unhashedBlockState

    getSlotBakers hbs slot = return $ case compare slotEpoch (epoch + 1) of
        -- LT should mean it's the current epoch, since the slot should be at least the slot of this block.
        LT -> epochToFullBakers (_unhashed (_birkCurrentEpochBakers bps))
        -- EQ means the next epoch.
        EQ -> epochToFullBakers (_unhashed (_birkNextEpochBakers bps))
        -- GT means a future epoch, so consider everything in the active bakers,
        -- applying any adjustments that occur in an epoch < slotEpoch.
        GT -> FullBakers {
                fullBakerInfos = futureBakers,
                bakerTotalStake = sum (_bakerStake <$> futureBakers)
            }
      where
        bs = _unhashedBlockState hbs
        bps = _blockBirkParameters bs
        SeedState{..} = _birkSeedState bps
        slotEpoch = fromIntegral $ slot `quot` epochLength
        futureBakers = Vec.fromList $ foldr resolveBaker [] (Set.toAscList (_activeBakers (_birkActiveBakers bps)))
        resolveBaker (BakerId aid) l = case bs ^? blockAccounts . Accounts.indexedAccount aid of
            Just acct -> case acct ^. accountBaker of
              Just AccountBaker{..} -> case _bakerPendingChange of
                RemoveBaker remEpoch
                  | remEpoch < slotEpoch -> l
                ReduceStake newAmt redEpoch
                  | redEpoch < slotEpoch -> (FullBakerInfo _accountBakerInfo newAmt) : l
                _ -> (FullBakerInfo _accountBakerInfo _stakedAmount) : l
              Nothing -> error "Basic.getSlotBakers invariant violation: active baker account not a baker"
            Nothing -> error "Basic.getSlotBakers invariant violation: active baker account not valid"

    {-# INLINE getRewardStatus #-}
    getRewardStatus = return . _unhashed . _blockBank . _unhashedBlockState

    {-# INLINE getTransactionOutcome #-}
    getTransactionOutcome bs trh =
        return $ bs ^? blockTransactionOutcomes . ix trh

    {-# INLINE getTransactionOutcomesHash #-}
    getTransactionOutcomesHash bs = return (getHash $ bs ^. blockTransactionOutcomes)

    {-# INLINE getStateHash #-}
    getStateHash = return . view blockStateHash

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

    {-# INLINE getElectionDifficulty #-}
    getElectionDifficulty bs ts = return (futureElectionDifficulty (bs ^. blockUpdates) ts)

    {-# INLINE getNextUpdateSequenceNumber #-}
    getNextUpdateSequenceNumber bs uty = return (lookupNextUpdateSequenceNumber (bs ^. blockUpdates) uty)

    {-# INLINE getCurrentElectionDifficulty #-}
    getCurrentElectionDifficulty bs = return (bs ^. blockUpdates . currentParameters . cpElectionDifficulty)

    {-# INLINE getUpdates #-}
    getUpdates bs = return (bs ^. blockUpdates)

    {-# INLINE getCryptographicParameters #-}
    getCryptographicParameters bs =
      return $! bs ^. blockCryptographicParameters . unhashed


instance Monad m => BS.AccountOperations (PureBlockStateMonad m) where

  getAccountAddress acc = return $ acc ^. accountAddress

  getAccountAmount acc = return $ acc ^. accountAmount

  getAccountNonce acc = return $ acc ^. accountNonce

  getAccountCredentials acc = return $ acc ^. accountCredentials

  getAccountMaxCredentialValidTo acc = return $ acc ^. accountMaxCredentialValidTo

  getAccountVerificationKeys acc = return $ acc ^. accountVerificationKeys

  getAccountEncryptedAmount acc = return $ acc ^. accountEncryptedAmount

  getAccountEncryptionKey acc = return $ acc ^. accountEncryptionKey

  getAccountReleaseSchedule acc = return $ acc ^. accountReleaseSchedule

  getAccountInstances acc = return $ acc ^. accountInstances

  getAccountBaker acc = return $ acc ^. accountBaker

instance Monad m => BS.BlockStateOperations (PureBlockStateMonad m) where

    {-# INLINE bsoGetModule #-}
    bsoGetModule bs mref = return $ bs ^. blockModules . to (Modules.getInterface mref)

    {-# INLINE bsoGetInstance #-}
    bsoGetInstance bs caddr = return (Instances.getInstance caddr (bs ^. blockInstances))

    {-# INLINE bsoGetAccount #-}
    bsoGetAccount bs aaddr =
      return $ bs ^? blockAccounts . ix aaddr

    {-# INLINE bsoGetAccountIndex #-}
    bsoGetAccountIndex bs aaddr = return $! Accounts.getAccountIndex aaddr (bs ^. blockAccounts)

    {-# INLINE bsoRegIdExists #-}
    bsoRegIdExists bs regid = return (Accounts.regIdExists regid (bs ^. blockAccounts))

    bsoCreateAccount bs gc keys addr cred = return $ 
            if Accounts.exists addr accounts then
              (Nothing, bs)
            else
              (Just acct, bs & blockAccounts .~ newAccounts)
        where
            acct = newAccount gc keys addr cred
            accounts = bs ^. blockAccounts
            newAccounts = Accounts.putAccount acct $
                          Accounts.recordRegId (regId cred)
                          accounts

    bsoPutNewInstance bs mkInstance = return (instanceAddress, bs')
        where
            (inst, instances') = Instances.createInstance mkInstance (bs ^. blockInstances)
            Instances.InstanceParameters{..} = Instances.instanceParameters inst
            bs' = bs
                -- Add the instance
                & blockInstances .~ instances'
                -- Update the owner account's set of instances
                & blockAccounts . ix instanceOwner . accountInstances %~ Set.insert instanceAddress

    bsoPutNewModule bs iface = return $!
        case Modules.putInterface iface (bs ^. blockModules) of
          Nothing -> (False, bs)
          Just mods' -> (True, bs & blockModules .~ mods')

    bsoModifyInstance bs caddr delta model = return $!
        bs & blockInstances %~ Instances.updateInstanceAt caddr delta model

    bsoModifyAccount bs accountUpdates = return $!
        -- Update the account
        (case accountUpdates ^. auCredential of
             Nothing -> bs & blockAccounts %~ Accounts.putAccount updatedAccount
             Just cdi ->
               bs & blockAccounts %~ Accounts.putAccount updatedAccount
                                   . Accounts.recordRegId (regId cdi))
        where
            account = bs ^. blockAccounts . singular (ix (accountUpdates ^. auAddress))
            updatedAccount = Accounts.updateAccount accountUpdates account

    {-# INLINE bsoNotifyEncryptedBalanceChange #-}
    bsoNotifyEncryptedBalanceChange bs amntDiff =
      return $! bs & blockBank . unhashed . Rewards.totalEncryptedGTU %~ applyAmountDelta amntDiff

    {-# INLINE bsoGetSeedState #-}
    bsoGetSeedState bs = return $! bs ^. blockBirkParameters . birkSeedState
    
    {-# INLINE bsoSetSeedState #-}
    bsoSetSeedState bs ss = return $! bs & blockBirkParameters . birkSeedState .~ ss

    bsoTransitionEpochBakers bs newEpoch = return $! newbs
        where
            oldBPs = bs ^. blockBirkParameters
            curActiveBIDs = Set.toDescList (oldBPs ^. birkActiveBakers . activeBakers)
            -- Add a baker to the accumulated set of bakers for the new next bakers
            accumBakers (bs0, bkrs0) bkr@(BakerId bid) = case bs ^? blockAccounts . Accounts.indexedAccount bid of
                Just acct -> case acct ^. accountBaker of
                  Just abkr@AccountBaker{..} -> case _bakerPendingChange of
                    RemoveBaker remEpoch
                      -- The baker will be removed in the next epoch, so do not add it to the list
                      | remEpoch == newEpoch + 1 -> (bs0, bkrs0)
                      -- The baker is now removed, so do not add it to the list and remove it as an active baker
                      | remEpoch <= newEpoch -> (
                              bs0
                                -- remove baker id and aggregation key from active bakers
                                & blockBirkParameters . birkActiveBakers . activeBakers %~ Set.delete bkr
                                & blockBirkParameters . birkActiveBakers . aggregationKeys %~ Set.delete (_bakerAggregationVerifyKey _accountBakerInfo)
                                -- remove the account's baker record
                                & blockAccounts . Accounts.indexedAccount bid %~ (accountBaker .~ Nothing),
                              bkrs0
                              )
                    ReduceStake newAmt redEpoch
                      -- Reduction takes effect in the next epoch
                      | redEpoch == newEpoch + 1 -> (bs0, (_accountBakerInfo, newAmt) : bkrs0)
                      -- Reduction is complete, so update the account accordingly.
                      | redEpoch <= newEpoch -> (
                              bs0
                                & blockAccounts . Accounts.indexedAccount bid %~
                                  (accountBaker ?~ abkr{_stakedAmount = newAmt, _bakerPendingChange = NoChange}),
                              (_accountBakerInfo, newAmt) : bkrs0
                              )
                    _ -> (bs0, (_accountBakerInfo, _stakedAmount) : bkrs0)
                  Nothing -> error "Basic.bsoTransitionEpochBakers invariant violation: active baker account not a baker"
                Nothing -> error "Basic.bsoTransitionEpochBakers invariant violation: active baker account not valid"
            (bs', bkrs) = foldl' accumBakers (bs, []) curActiveBIDs
            newNextBakers = makeHashed $ EpochBakers {
              _bakerInfos = Vec.fromList (fst <$> bkrs),
              _bakerStakes = Vec.fromList (snd <$> bkrs),
              _bakerTotalStake = foldl' (+) 0 (snd <$> bkrs)
            }
            newbs = bs' & blockBirkParameters %~ \bp -> bp {
                        _birkCurrentEpochBakers = _birkNextEpochBakers bp,
                        _birkNextEpochBakers = newNextBakers
                    }

    bsoAddBaker bs aaddr BakerAdd{..} = return $! case Accounts.getAccountWithIndex aaddr (bs ^. blockAccounts) of
        -- Cannot resolve the account
        Nothing -> (BAInvalidAccount, bs)
        -- Account is already a baker
        Just (ai, Account{_accountBaker = Just _}) -> (BAAlreadyBaker (BakerId ai), bs)
        Just (ai, Account{..})
          -- Aggregation key is a duplicate
          | bkuAggregationKey baKeys `Set.member` (bs ^. blockBirkParameters . birkActiveBakers . aggregationKeys) -> (BADuplicateAggregationKey, bs)
          -- All checks pass, add the baker
          | otherwise -> let bid = BakerId ai in
              (BASuccess bid, bs
                & blockAccounts . Accounts.indexedAccount ai . accountBaker ?~ AccountBaker{
                  _stakedAmount = baStake,
                  _stakeEarnings = baStakeEarnings,
                  _accountBakerInfo = bakerKeyUpdateToInfo bid baKeys,
                  _bakerPendingChange = NoChange
                }
                & blockBirkParameters . birkActiveBakers . aggregationKeys %~ Set.insert (bkuAggregationKey baKeys)
                & blockBirkParameters . birkActiveBakers . activeBakers %~ Set.insert bid
                )

    bsoUpdateBakerKeys bs aaddr bku@BakerKeyUpdate{..} = return $! case Accounts.getAccountWithIndex aaddr (bs ^. blockAccounts) of
        -- The account is valid and has a baker
        Just (ai, Account{_accountBaker = Just ab@AccountBaker{..}})
          -- The key would duplicate an existing aggregation key (other than the baker's current key)
          | bkuAggregationKey /= _bakerAggregationVerifyKey _accountBakerInfo
          , bkuAggregationKey `Set.member` (bs ^. blockBirkParameters . birkActiveBakers . aggregationKeys) -> (BKUDuplicateAggregationKey, bs)
          -- The aggregation key is not a duplicate, so update the baker
          | otherwise -> (BKUSuccess (BakerId ai), bs
              & blockAccounts . Accounts.indexedAccount ai . accountBaker ?~
                ab{_accountBakerInfo = bakerKeyUpdateToInfo (_accountBakerInfo ^. bakerIdentity) bku}
              & blockBirkParameters . birkActiveBakers . aggregationKeys %~ Set.insert bkuAggregationKey . Set.delete (_bakerAggregationVerifyKey _accountBakerInfo)
              )
        -- Cannot resolve the account, or it is not a baker
        _ -> (BKUInvalidBaker, bs)

    bsoUpdateBakerStake bs aaddr newStake = return $! case Accounts.getAccountWithIndex aaddr (bs ^. blockAccounts) of
        -- The account is valid and has a baker
        Just (ai, Account{_accountBaker = Just ab@AccountBaker{..}, ..})
          -- A change is already pending
          | _bakerPendingChange /= NoChange -> (BSUChangePending (BakerId ai), bs)
          -- We can make the change
          | otherwise ->
              let (res, updateStake) = case compare newStake _stakedAmount of
                          LT -> let curEpoch = epoch $ _birkSeedState $ _blockBirkParameters bs
                                    cooldown = 2 + bs ^. blockUpdates . currentParameters . cpBakerExtraCooldownEpochs
                                in (BSUStakeReduced (BakerId ai) (curEpoch + cooldown), bakerPendingChange .~ ReduceStake newStake (curEpoch + cooldown))
                          EQ -> (BSUStakeUnchanged (BakerId ai), id)
                          GT -> (BSUStakeIncreased (BakerId ai), stakedAmount .~ newStake)
              in (res, bs & blockAccounts . Accounts.indexedAccount ai . accountBaker ?~ (ab & updateStake))
        -- The account is not valid or has no baker
        _ -> (BSUInvalidBaker, bs)

    bsoUpdateBakerRestakeEarnings bs aaddr newRestakeEarnings = return $! case Accounts.getAccountWithIndex aaddr (bs ^. blockAccounts) of
        -- The account is valid and has a baker
        Just (ai, Account{_accountBaker = Just ab@AccountBaker{..}, ..}) ->
          if newRestakeEarnings == _stakeEarnings
          -- No actual change
          then (BREUUpdated (BakerId ai), bs)
          -- A real change
          else (BREUUpdated (BakerId ai), bs & blockAccounts . Accounts.indexedAccount ai . accountBaker ?~ ab{_stakeEarnings = newRestakeEarnings})
        _ -> (BREUInvalidBaker, bs)

    bsoRemoveBaker bs aaddr = return $! case Accounts.getAccountWithIndex aaddr (bs ^. blockAccounts) of
        -- The account is valid and has a baker
        Just (ai, Account{_accountBaker = Just ab@AccountBaker{..}, ..})
          -- A change is already pending
          | _bakerPendingChange /= NoChange -> (BRChangePending (BakerId ai), bs)
          -- We can make the change
          | otherwise ->
              let curEpoch = epoch $ _birkSeedState $ _blockBirkParameters bs
                  cooldown = 2 + bs ^. blockUpdates . currentParameters . cpBakerExtraCooldownEpochs
              in (BRRemoved (BakerId ai) (curEpoch + cooldown), 
                  bs & blockAccounts . Accounts.indexedAccount ai . accountBaker ?~ (ab & bakerPendingChange .~ RemoveBaker (curEpoch + cooldown)))
        -- The account is not valid or has no baker
        _ -> (BRInvalidBaker, bs)

    -- This uses that baker identities are account indexes.  The account with the corresponding
    -- index (if any) is given the reward.  If the account has a baker (which it presumably should) then
    -- the stake is increased correspondingly if 'stakeEarnings' is set.
    bsoRewardBaker bs (BakerId ai) !reward = return (getFirst <$> mfaddr, bs')
      where
        (mfaddr, !bs') = bs & (blockAccounts . Accounts.indexedAccount ai) payReward
        payReward acct = (Just . First $! acct ^. accountAddress, acct & accountAmount +~ reward & accountBaker . traversed %~ updateBaker)
        updateBaker bkr
          | _stakeEarnings bkr = bkr & stakedAmount +~ reward
          | otherwise = bkr

    bsoRewardFoundationAccount bs !reward = return $ bs & blockAccounts . Accounts.indexedAccount foundationAccount . accountAmount +~ reward
      where
        foundationAccount = bs ^. blockUpdates . currentParameters . cpFoundationAccount
    
    bsoGetFoundationAccount bs = return $ bs ^?! blockAccounts . Accounts.indexedAccount foundationAccount
      where
        foundationAccount = bs ^. blockUpdates . currentParameters . cpFoundationAccount

    -- mint currency, distributing it to the reward accounts and foundation account,
    -- updating the total GTU.
    bsoMint bs mint = return $
        bs
        & blockBank . unhashed %~ updateBank
        & blockAccounts . Accounts.indexedAccount foundationAccount . accountAmount +~ BS.mintDevelopmentCharge mint
      where
        updateBank = (Rewards.totalGTU +~ BS.mintTotal mint)
                . (Rewards.bakingRewardAccount +~ BS.mintBakingReward mint)
                . (Rewards.finalizationRewardAccount +~ BS.mintFinalizationReward mint)
        foundationAccount = bs ^. blockUpdates . currentParameters . cpFoundationAccount

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
      return $! bs & blockTransactionOutcomes . Transactions.outcomeSpecial %~ (Seq.|> o)

    {-# INLINE bsoProcessUpdateQueues #-}
    bsoProcessUpdateQueues bs ts = return (changes, bs & blockUpdates .~ newBlockUpdates)
      where
        (!changes, !newBlockUpdates) = processUpdateQueues ts (bs ^. blockUpdates)

    {-# INLINE bsoProcessReleaseSchedule #-}
    bsoProcessReleaseSchedule bs ts = do
      let (accountsToRemove, blockReleaseSchedule') = Map.partition (<= ts) $ bs ^. blockReleaseSchedule
      if Map.null accountsToRemove
        then return bs
        else
        let f (ba, brs) addr =
              let mUnlocked = unlockAmountsUntil ts <$> (ba ^? ix addr . accountReleaseSchedule)
              in
                case mUnlocked of
                  Nothing -> (ba, brs)
                  Just (_, newTs, ars') ->
                    let ba' = ba & ix addr . accountReleaseSchedule .~ ars'
                        brs' = case newTs of
                                 Just k -> Map.insert addr k brs
                                 Nothing -> brs
                    in (ba', brs')
            (blockAccounts', blockReleaseSchedule'') = foldl' f (bs ^. blockAccounts, blockReleaseSchedule') (Map.keys accountsToRemove)
        in
          return $! bs & blockAccounts .~ blockAccounts'
                       & blockReleaseSchedule .~ blockReleaseSchedule''


    {-# INLINE bsoGetCurrentAuthorizations #-}
    bsoGetCurrentAuthorizations bs = return $! bs ^. blockUpdates . currentAuthorizations . unhashed

    {-# INLINE bsoGetNextUpdateSequenceNumber #-}
    bsoGetNextUpdateSequenceNumber bs uty = return $! lookupNextUpdateSequenceNumber (bs ^. blockUpdates) uty

    {-# INLINE bsoEnqueueUpdate #-}
    bsoEnqueueUpdate bs effectiveTime payload = return $! bs & blockUpdates %~ enqueueUpdate effectiveTime payload

    {-# INLINE bsoAddReleaseSchedule #-}
    bsoAddReleaseSchedule bs rel = do
      let f relSchedule (addr, t) = Map.alter (\case
                                                  Nothing -> Just t
                                                  Just t' -> Just $ min t' t) addr relSchedule
          updateBRS brs = foldl' f brs rel
      return $! bs & blockReleaseSchedule %~ updateBRS

    {-# INLINE bsoGetEnergyRate #-}
    bsoGetEnergyRate bs = return $! bs ^. blockUpdates . currentParameters . cpEnergyRate

    bsoGetChainParameters bs = return $! bs ^. blockUpdates . currentParameters

    bsoGetEpochBlocksBaked bs = return $! (_2 %~ Map.toList) (foldl' accumBakers (0, Map.empty) (bs ^. blockEpochBlocksBaked . to hebBlocks))
      where
        accumBakers (t, m) b =
          let !t' = t + 1
              !m' = m & at b . non 0 +~ 1
          in (t', m' )

    bsoNotifyBlockBaked bs bid = return $! bs & blockEpochBlocksBaked %~ consEpochBlock bid

    bsoClearEpochBlocksBaked bs = return $! bs & blockEpochBlocksBaked .~ emptyHashedEpochBlocks

    bsoGetBankStatus bs = return $! bs ^. blockBank . unhashed

    bsoSetRewardAccounts bs rew = return $! bs & blockBank . unhashed . Rewards.rewardAccounts .~ rew

instance Monad m => BS.BlockStateStorage (PureBlockStateMonad m) where
    {-# INLINE thawBlockState #-}
    thawBlockState bs = return $ _unhashedBlockState bs

    {-# INLINE freezeBlockState #-}
    freezeBlockState bs = return $! hashBlockState bs

    {-# INLINE dropUpdatableBlockState #-}
    dropUpdatableBlockState _ = return ()

    {-# INLINE purgeBlockState #-}
    purgeBlockState _ = return ()

    {-# INLINE archiveBlockState #-}
    archiveBlockState _ = return ()

    {-# INLINE saveBlockState #-}
    saveBlockState _ = return ()

    {-# INLINE loadBlockState #-}
    loadBlockState _ _ = error "Cannot load memory-based block state"

    {-# INLINE cacheBlockState #-}
    cacheBlockState = return

-- |Initial block state.
initialState :: SeedState
             -> CryptographicParameters
             -> [Account]
             -> IPS.IdentityProviders
             -> ARS.AnonymityRevokers
             -> Authorizations
             -> ChainParameters
             -> BlockState
initialState seedState cryptoParams genesisAccounts ips anonymityRevokers auths chainParams = BlockState {..}
  where
    _blockBirkParameters = initialBirkParameters genesisAccounts seedState
    _blockCryptographicParameters = makeHashed cryptoParams
    _blockAccounts = List.foldl' (flip Accounts.putAccountWithRegIds) Accounts.emptyAccounts genesisAccounts
    _blockInstances = Instances.emptyInstances
    _blockModules = Modules.emptyModules
    -- initial amount in the central bank is the amount on all genesis accounts combined
    initialAmount = List.foldl' (\c acc -> c + acc ^. accountAmount) 0 $ genesisAccounts
    _blockBank = makeHashed $ Rewards.makeGenesisBankStatus initialAmount
    _blockIdentityProviders = makeHashed ips
    _blockAnonymityRevokers = makeHashed anonymityRevokers
    _blockTransactionOutcomes = Transactions.emptyTransactionOutcomes
    _blockUpdates = initialUpdates auths chainParams
    _blockReleaseSchedule = Map.empty
    _blockEpochBlocksBaked = emptyHashedEpochBlocks
    _blockStateHash = BS.makeBlockStateHash BS.BlockStateHashInputs {
              bshBirkParameters = getHash _blockBirkParameters,
              bshCryptographicParameters = getHash _blockCryptographicParameters,
              bshIdentityProviders = getHash _blockIdentityProviders,
              bshAnonymityRevokers = getHash _blockAnonymityRevokers,
              bshModules = getHash _blockModules,
              bshBankStatus = getHash _blockBank,
              bshAccounts = getHash _blockAccounts,
              bshInstances = getHash _blockInstances,
              bshUpdates = getHash _blockUpdates,
              bshEpochBlocks = getHash _blockEpochBlocksBaked
            }

-- |Initial block state based on 'GenesisData'.
genesisState :: GenesisData -> BlockState
genesisState GenesisDataV2{..} = BlockState {..}
  where
    mkAccount GenesisAccount{..} bid =
          case gaBaker of
            Just GenesisBaker{..} | gbBakerId /= bid -> error "Mismatch between assigned and chosen baker id."
            _ -> newAccountMultiCredential genesisCryptographicParameters gaVerifyKeys gaAddress gaCredentials
                      & accountAmount .~ gaBalance
                      & case gaBaker of
                          Nothing -> id
                          Just GenesisBaker{..} -> accountBaker ?~ AccountBaker {
                            _stakedAmount = gbStake,
                            _stakeEarnings = gbRestakeEarnings,
                            _accountBakerInfo = BakerInfo {
                                _bakerIdentity = bid,
                                _bakerSignatureVerifyKey = gbSignatureVerifyKey,
                                _bakerElectionVerifyKey = gbElectionVerifyKey,
                                _bakerAggregationVerifyKey = gbAggregationVerifyKey
                                },
                            _bakerPendingChange = NoChange
                            }
    accounts = zipWith mkAccount genesisAccounts [0..]
    _blockBirkParameters = initialBirkParameters accounts genesisSeedState
    _blockCryptographicParameters = makeHashed genesisCryptographicParameters
    _blockAccounts = List.foldl' (flip Accounts.putAccountWithRegIds) Accounts.emptyAccounts accounts
    _blockInstances = Instances.emptyInstances
    _blockModules = Modules.emptyModules
    -- initial amount in the central bank is the amount on all genesis accounts combined
    initialAmount = List.foldl' (\c acc -> c + acc ^. accountAmount) 0 accounts
    _blockBank = makeHashed $ Rewards.makeGenesisBankStatus initialAmount
    _blockIdentityProviders = makeHashed genesisIdentityProviders
    _blockAnonymityRevokers = makeHashed genesisAnonymityRevokers
    _blockTransactionOutcomes = Transactions.emptyTransactionOutcomes
    _blockUpdates = initialUpdates genesisAuthorizations genesisChainParameters
    _blockReleaseSchedule = Map.empty
    _blockEpochBlocksBaked = emptyHashedEpochBlocks
    _blockStateHash = BS.makeBlockStateHash BS.BlockStateHashInputs {
              bshBirkParameters = getHash _blockBirkParameters,
              bshCryptographicParameters = getHash _blockCryptographicParameters,
              bshIdentityProviders = getHash _blockIdentityProviders,
              bshAnonymityRevokers = getHash _blockAnonymityRevokers,
              bshModules = getHash _blockModules,
              bshBankStatus = getHash _blockBank,
              bshAccounts = getHash _blockAccounts,
              bshInstances = getHash _blockInstances,
              bshUpdates = getHash _blockUpdates,
              bshEpochBlocks = getHash _blockEpochBlocksBaked
            }
