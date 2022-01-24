{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Concordium.GlobalState.Basic.BlockState where

import qualified Data.Map as LazyMap
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
import Data.ByteString.Builder (hPutBuilder)
import Control.Monad.IO.Class
import qualified Control.Monad.State.Strict as MTL
import qualified Control.Monad.Except as MTL
import qualified Control.Monad.Writer.Strict as MTL

import Concordium.Types
import Concordium.Types.Accounts
import Concordium.Types.Updates
import Concordium.Types.UpdateQueues
import Concordium.Types.Execution
import qualified Concordium.Genesis.Data as GenesisData
import qualified Concordium.Genesis.Data.P1 as P1
import qualified Concordium.Genesis.Data.P2 as P2
import qualified Concordium.Genesis.Data.P3 as P3
import qualified Concordium.GlobalState.Types as GT
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.AccountTransactionIndex
--import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Basic.BlockState.Bakers
import qualified Concordium.GlobalState.BlockState as BS
import Concordium.GlobalState.Basic.BlockState.Account
import qualified Concordium.GlobalState.Wasm as GSWasm
import qualified Concordium.GlobalState.Basic.BlockState.Accounts as Accounts
import qualified Concordium.GlobalState.Basic.BlockState.Modules as Modules
import qualified Concordium.GlobalState.Basic.BlockState.Instances as Instances
import qualified Concordium.GlobalState.AccountMap as AccountMap
import qualified Concordium.GlobalState.Rewards as Rewards
import qualified Concordium.Types.IdentityProviders as IPS
import qualified Concordium.Types.AnonymityRevokers as ARS
import Concordium.GlobalState.Basic.BlockState.Updates
import qualified Concordium.Types.Transactions as Transactions
import Concordium.GlobalState.Basic.BlockState.AccountReleaseSchedule
import Concordium.Types.SeedState
import Concordium.ID.Types (credId)
import qualified Concordium.Crypto.SHA256 as H
import Concordium.Types.HashableTo

import Concordium.Utils
import Concordium.Utils.Serialization

data NextEpochBakers (av :: AccountVersion) where
    -- |The 'EpochBakers' for the next epoch.
    NextEpochBakers :: !(Hashed EpochBakers) -> NextEpochBakers av
    -- |The next epoch does not require a change in the 'EpochBakers'.
    UnchangedNextEpochBakers :: NextEpochBakers 'AccountV1

deriving instance Eq (NextEpochBakers av)
deriving instance Show (NextEpochBakers av)

data BasicBirkParameters (av :: AccountVersion) = BasicBirkParameters {
    -- |The currently-registered bakers.
    -- For AccountV0, bakers remain active while they are current epoch bakers (or waiting to become
    -- current). From AccountV1 onwards, bakers may remain current epoch bakers after they have been
    -- removed from the active bakers.
    -- Invariant: the active bakers correspond exactly to the accounts that have baker records.
    -- (And delegators likewise.)
    _birkActiveBakers :: !ActiveBakers,
    -- |The bakers that will be used for the next epoch.
    _birkNextEpochBakers :: !(NextEpochBakers av),
    -- |The bakers for the current epoch.
    _birkCurrentEpochBakers :: !(Hashed EpochBakers),
    -- |The seed state used to derive the leadership election nonce.
    _birkSeedState :: !SeedState
} deriving (Eq, Show)

-- |The hash of the birk parameters derives from the seed state
-- and the bakers for the current and next epochs.  The active
-- bakers are not included, because they derive from the accounts.
--
-- From protocol version 'P4' onwards, it is possible for there to be no next epoch bakers, in which
-- case, the hash of the empty string is used for the hash of the next epoch bakers. (This should
-- not collide with the hash of an actual 'EpochBakers'.)
instance HashableTo H.Hash (BasicBirkParameters av) where
    getHash BasicBirkParameters {..} = H.hashOfHashes bpH0 bpH1
      where
        bpH0 = H.hash $ "SeedState" <> encode _birkSeedState
        bpH1 = H.hashOfHashes nebHash cebHash
        nebHash = case _birkNextEpochBakers of
            NextEpochBakers heb -> getHash heb
            UnchangedNextEpochBakers -> H.hash ""
        cebHash = getHash _birkCurrentEpochBakers

-- |Serialize 'BasicBirkParameters' in V0 format.
putBirkParameters :: forall av. (IsAccountVersion av) => Putter (BasicBirkParameters av)
putBirkParameters BasicBirkParameters{..} = do
    put _birkSeedState
    (_ :: ()) <- case accountVersion @av of
        SAccountV0 -> case _birkNextEpochBakers :: NextEpochBakers 'AccountV0 of
            NextEpochBakers neb -> putEpochBakers (_unhashed neb)
        SAccountV1 -> case _birkNextEpochBakers :: NextEpochBakers 'AccountV1 of
            NextEpochBakers neb -> putWord8 1 >> putEpochBakers (_unhashed neb)
            UnchangedNextEpochBakers -> putWord8 0
    putEpochBakers (_unhashed _birkCurrentEpochBakers)

-- |Deserialize 'BasicBirkParameters' in V0 format.
-- Since the active bakers are not stored in the serialization,
-- the 'BasicBirkParameters' will have empty 'ActiveBakers',
-- which should be corrected by processing the accounts table.
getBirkParameters :: forall av. (IsAccountVersion av) => Get (BasicBirkParameters av)
getBirkParameters = do
    let accVer = accountVersion @av
    _birkSeedState <- get
    _birkNextEpochBakers <- case accVer of
        SAccountV0 -> NextEpochBakers . makeHashed <$> getEpochBakers
        SAccountV1 -> getWord8 >>= \case
            0 -> return UnchangedNextEpochBakers
            1 -> NextEpochBakers . makeHashed <$> getEpochBakers
            _ -> fail "Unsupported next epoch bakers"
    _birkCurrentEpochBakers <- makeHashed <$> getEpochBakers
    let _birkActiveBakers = ActiveBakers Map.empty Set.empty Set.empty
    return BasicBirkParameters{..}

-- |Generate initial birk parameters from genesis accounts and seed state.
initialBirkParameters
  :: forall av
   . [Account av]
  -- ^The accounts at genesis, in order
  -> SeedState
  -- ^The seed state
  -> BasicBirkParameters av
initialBirkParameters accounts _birkSeedState = BasicBirkParameters{..}
  where
    alterDel del Nothing = Just $ Set.singleton del
    alterDel del (Just dels) = Just $ Set.insert del dels
    addBakerDel :: Map.Map BakerId (Set.Set DelegatorId) -> Account av -> Map.Map BakerId (Set.Set DelegatorId)
    addBakerDel bdmap acct =
        case acct ^. accountStaking of
            AccountStakeDelegate AccountDelegationV1{..} ->
                case _delegationTarget of
                    DelegateToLPool -> bdmap
                    DelegateToBaker bid -> Map.alter (alterDel _delegationIdentity) bid bdmap
            _ -> bdmap
    addLPoolDel :: Set.Set DelegatorId -> Account av -> Set.Set DelegatorId
    addLPoolDel dels acct =
        case acct ^. accountStaking of
            AccountStakeDelegate AccountDelegationV1{..} ->
                case _delegationTarget of
                    DelegateToLPool -> Set.insert _delegationIdentity dels
                    DelegateToBaker _ -> dels
            _ -> dels
    bakerDelsMap = foldl addBakerDel Map.empty accounts
    lookupBakerDels bid =
        case Map.lookup bid bakerDelsMap of
            Nothing -> Set.empty
            Just dels -> dels
    abi (AccountStakeBaker AccountBaker{..}) = Just (_accountBakerInfo ^. bakerInfo, _stakedAmount)
    abi _ = Nothing
    bkr acct = abi $ acct ^. accountStaking
    bkrs = catMaybes $ bkr <$> accounts
    _birkActiveBakers = ActiveBakers {
      _activeBakers = Map.fromList $ bkrs <&> \(BakerInfo{_bakerIdentity = bi}, _) -> (bi, lookupBakerDels bi),
      _aggregationKeys = Set.fromList (_bakerAggregationVerifyKey . fst <$> bkrs),
      _lPoolDelegators = foldl addLPoolDel Set.empty accounts
    }
    stakes = Vec.fromList (snd <$> bkrs)
    _birkCurrentEpochBakers = makeHashed $ EpochBakers {
      _bakerInfos = Vec.fromList (fst <$> bkrs),
      _bakerStakes = stakes,
      _bakerTotalStake = sum stakes
    }
    _birkNextEpochBakers = NextEpochBakers _birkCurrentEpochBakers

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

-- |Serialize 'HashedEpochBlocks' in V0 format.
putHashedEpochBlocksV0 :: Putter HashedEpochBlocks
putHashedEpochBlocksV0 HashedEpochBlocks{..} = do
    putLength (length hebBlocks)
    mapM_ put hebBlocks

-- |Deserialize 'HashedEpochBlocks' in V0 format.
getHashedEpochBlocksV0 :: Get HashedEpochBlocks
getHashedEpochBlocksV0 = do
    numBlocks <- getLength
    blocks <- replicateM numBlocks get
    return $! foldr' consEpochBlock emptyHashedEpochBlocks blocks

data BlockState (pv :: ProtocolVersion) = BlockState {
    _blockAccounts :: !(Accounts.Accounts pv),
    _blockInstances :: !Instances.Instances,
    _blockModules :: !Modules.Modules,
    _blockBank :: !(Hashed Rewards.BankStatus),
    _blockIdentityProviders :: !(Hashed IPS.IdentityProviders),
    _blockAnonymityRevokers :: !(Hashed ARS.AnonymityRevokers),
    _blockBirkParameters :: !(BasicBirkParameters (AccountVersionFor pv)),
    _blockCryptographicParameters :: !(Hashed CryptographicParameters),
    _blockUpdates :: !(Updates pv),
    _blockReleaseSchedule :: !(LazyMap.Map AccountAddress Timestamp), -- ^Contains an entry for each account that has pending releases and the first timestamp for said account
    _blockTransactionOutcomes :: !Transactions.TransactionOutcomes,
    _blockEpochBlocksBaked :: !HashedEpochBlocks
} deriving (Show)

data HashedBlockState pv = HashedBlockState {
    _unhashedBlockState :: !(BlockState pv),
    _blockStateHash :: !StateHash
} deriving (Show)

makeLenses ''BasicBirkParameters
makeClassy ''BlockState
makeLenses ''HashedBlockState

instance IsProtocolVersion pv => HasBlockState (HashedBlockState pv) pv where
    blockState = unhashedBlockState

instance HashableTo StateHash (HashedBlockState pv) where
    getHash = _blockStateHash

-- |Construct a block state that is empty, except for the supplied 'BirkParameters',
-- 'CryptographicParameters', 'Authorizations' and 'ChainParameters'.
emptyBlockState
    :: IsProtocolVersion pv =>
    BasicBirkParameters (AccountVersionFor pv) ->
    CryptographicParameters ->
    UpdateKeysCollection (ChainParametersVersionFor pv) ->
    ChainParameters pv ->
    BlockState pv
emptyBlockState _blockBirkParameters cryptographicParameters keysCollection chainParams = BlockState
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
      _blockUpdates = initialUpdates keysCollection chainParams
      _blockReleaseSchedule = Map.empty
      _blockEpochBlocksBaked = emptyHashedEpochBlocks


hashBlockState :: forall pv. IsProtocolVersion pv => BlockState pv -> HashedBlockState pv
hashBlockState = case protocolVersion :: SProtocolVersion pv of
  SP1 -> hashBlockStateP1
  SP2 -> hashBlockStateP1
  SP3 -> hashBlockStateP1
    -- For protocol versions P1, P2, and P3, convert a @BlockState pv@ to a
    -- @HashedBlockState pv@ by computing the state hash. The state and hashing
    -- is the same. This function was introduced in protocol version 1 which is
    -- reflected in its name.
  where hashBlockStateP1 bs@BlockState{..} = HashedBlockState {
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


instance IsProtocolVersion pv => HashableTo StateHash (BlockState pv) where
    getHash = _blockStateHash . hashBlockState


-- |Serialize 'BlockState'. The format may depend on the protocol version.
putBlockState :: IsProtocolVersion pv => Putter (BlockState pv)
putBlockState bs = do
    -- BirkParameters
    putBirkParameters (bs ^. blockBirkParameters)
    -- CryptographicParameters
    let cryptoParams = bs ^. blockCryptographicParameters . unhashed
    put cryptoParams
    -- IdentityProviders
    put (bs ^. blockIdentityProviders . unhashed)
    -- AnonymityRevokers
    put (bs ^. blockAnonymityRevokers . unhashed)
    -- Modules
    Modules.putModulesV0 (bs ^. blockModules)
    -- BankStatus
    put (bs ^. blockBank . unhashed)
    -- Accounts
    Accounts.serializeAccounts cryptoParams (bs ^. blockAccounts)
    -- Instances
    Instances.putInstancesV0 (bs ^. blockInstances)
    -- Updates
    putUpdatesV0 (bs ^. blockUpdates)
    -- Epoch blocks
    putHashedEpochBlocksV0 (bs ^. blockEpochBlocksBaked)

-- |Deserialize 'BlockState'. The format may depend on the protocol version.
-- This checks the following invariants:
--
--  * Bakers cannot have duplicate aggregation keys.
--
-- The serialization format reduces redundancy to ensure that:
--
--  * The active bakers are exactly the accounts with baker records.
--  * The block release schedule contains the minimal scheduled release
--    timestamp for every account with a scheduled release.
--
-- Note that the transaction outcomes will always be empty.
getBlockState :: forall oldpv pv. IsProtocolVersion pv => StateMigrationParameters oldpv pv -> Get (BlockState pv)
getBlockState migration = do
    -- BirkParameters
    preBirkParameters <- getBirkParameters
    -- CryptographicParameters
    cryptoParams <- get
    let _blockCryptographicParameters = makeHashed cryptoParams
    -- IdentityProviders
    _blockIdentityProviders <- makeHashed <$> get
    -- AnonymityRevokers
    _blockAnonymityRevokers <- makeHashed <$> get
    -- Modules
    _blockModules <- Modules.getModulesV0
    -- BankStatus
    _blockBank <- makeHashed <$> get
    (_blockAccounts :: Accounts.Accounts pv) <- Accounts.deserializeAccounts migration cryptoParams
    let resolveModule modRef initName = do
            mi <- Modules.getInterface modRef _blockModules
            return (GSWasm.miExposedReceive mi ^. at initName . non Set.empty, mi)
    _blockInstances <- Instances.getInstancesV0 resolveModule
    _blockUpdates <- getUpdatesV0
    _blockEpochBlocksBaked <- getHashedEpochBlocksV0
    -- Construct the release schedule and active bakers from the accounts
    let processBakerAccount (rs,bkrs) account = do
          let rs' = case Map.minViewWithKey (account ^. accountReleaseSchedule . pendingReleases) of
                  Nothing -> rs
                  Just ((ts, _), _) -> Map.insert (account ^. accountAddress) ts rs
          bkrs' <-case account ^. accountStaking of
            AccountStakeBaker AccountBaker {_accountBakerInfo = abi} -> do
                when ((abi ^. bakerAggregationVerifyKey) `Set.member` _aggregationKeys bkrs) $
                  fail "Duplicate baker aggregation key"
                return $! bkrs & activeBakers %~ Map.insert (abi ^. bakerIdentity) Set.empty
                          & aggregationKeys %~ Set.insert (abi ^. bakerAggregationVerifyKey)
            _ -> return bkrs
          return (rs', bkrs')
    let processDelegatorAccount :: ActiveBakers -> Account (AccountVersionFor pv) -> Get ActiveBakers
        processDelegatorAccount bkrs account =
          case account ^. accountStaking of
            AccountStakeDelegate AccountDelegationV1{..} -> do
              case _delegationTarget of
                DelegateToLPool ->
                  return $! bkrs & lPoolDelegators %~ Set.insert _delegationIdentity
                DelegateToBaker bid ->
                  case Map.lookup bid (bkrs ^. activeBakers)  of
                    Nothing -> fail "Missing delegation target baker"
                    Just dels -> do
                      let newDels = Set.insert _delegationIdentity dels
                      return $! bkrs & activeBakers %~ Map.insert bid newDels
            _ -> return bkrs
    (_blockReleaseSchedule, preActBkrs) <- foldM processBakerAccount (Map.empty, _birkActiveBakers preBirkParameters) (Accounts.accountList _blockAccounts)
    actBkrs <- foldM processDelegatorAccount preActBkrs (Accounts.accountList _blockAccounts)
    let _blockBirkParameters = preBirkParameters {_birkActiveBakers = actBkrs}
    let _blockTransactionOutcomes = Transactions.emptyTransactionOutcomes
    return BlockState{..}

-- | Fold left over delegators for a given baker
-- 'activeBakerFoldlDelegators' @bs@ @f@ @a@ @bid@, where
-- * @bs@ is used to lookup the delegator accounts and active bakers,
-- * @f@ is accumulation function,
-- * @a@ is the initial value,
-- * @bid@ is the baker.
-- If @bid@ is not an active baker in @bs@, then the initial value @a@ is returned. It is assumed
-- that all delegators to the baker @bid@ are delegator accounts in @bs@.
activeBakerFoldlDelegators
    :: IsProtocolVersion pv
    => BlockState pv
    -> (a -> DelegatorId -> AccountDelegation (AccountVersionFor pv) -> a)
    -> a
    -> BakerId
    -> a
activeBakerFoldlDelegators bs f a0 bid = do
    case Map.lookup bid (bs ^. blockBirkParameters . birkActiveBakers . activeBakers) of
        Just dset -> foldl faccount a0 dset
        _ -> a0
    where
      faccount a did@(DelegatorId aid) =
        case bs ^? blockAccounts . Accounts.indexedAccount aid of
            Just Account{_accountStaking = AccountStakeDelegate acctDel} ->
                f a did acctDel
            _ ->
                error "Invariant violation: active delegator account not a valid delegator"

-- | Get total pool capital, sum of baker and delegator stakes,
-- 'totalPoolCapital' @bs@ @bid@, where
-- * @bs@ is used to lookup accounts and active bakers,
-- * @bid@ is the baker.
-- If @bid@ is not a baker in @accounts@, then @0@ is returned.
-- If @bid@ is not an active baker in @ab@, then the baker's equity capital (stake) is returned.
-- It is assumed that all delegators to the baker @bid@ are delegator accounts in @accounts@.
totalPoolCapital
    :: IsProtocolVersion pv
    => BlockState pv
    -> BakerId
    -> Amount
totalPoolCapital bs bid@(BakerId aid) = do
    case bs ^? blockAccounts . Accounts.indexedAccount aid of
        Just Account{_accountStaking = AccountStakeBaker acctBkr} ->
            activeBakerFoldlDelegators bs addDelStake (acctBkr ^. stakedAmount) bid
        _ ->
            0
    where
      addDelStake :: Amount -> DelegatorId -> AccountDelegation av -> Amount
      addDelStake a _ AccountDelegationV1{..} = a + _delegationStakedAmount

newtype PureBlockStateMonad (pv :: ProtocolVersion) m a = PureBlockStateMonad {runPureBlockStateMonad :: m a}
    deriving (Functor, Applicative, Monad)

type instance GT.BlockStatePointer (BlockState pv) = ()
type instance GT.BlockStatePointer (HashedBlockState pv) = ()

instance IsProtocolVersion pv => GT.MonadProtocolVersion (PureBlockStateMonad pv m) where
  type MPV (PureBlockStateMonad pv m) = pv

instance GT.BlockStateTypes (PureBlockStateMonad pv m) where
    type BlockState (PureBlockStateMonad pv m) = HashedBlockState pv
    type UpdatableBlockState (PureBlockStateMonad pv m) = BlockState pv
    type Account (PureBlockStateMonad pv m) = Account (AccountVersionFor pv)
    type BakerInfoRef (PureBlockStateMonad pv m) = BakerInfo

instance ATITypes (PureBlockStateMonad pv m) where
  type ATIStorage (PureBlockStateMonad pv m) = ()

instance Monad m => PerAccountDBOperations (PureBlockStateMonad pv m)
  -- default implementation

instance (IsProtocolVersion pv, Monad m) => BS.BlockStateQuery (PureBlockStateMonad pv m) where
    {-# INLINE getModule #-}
    getModule bs mref =
        return $ bs ^. blockModules . to (Modules.getSource mref)

    {-# INLINE getContractInstance #-}
    getContractInstance bs caddr = return (Instances.getInstance caddr (bs ^. blockInstances))

    {-# INLINE getAccount #-}
    getAccount bs aaddr = return $!
        case Accounts.getAccountIndex aaddr (bs ^. blockAccounts) of
          Nothing -> Nothing
          Just ai -> (ai, ) <$> (bs ^? blockAccounts . Accounts.indexedAccount ai)

    getActiveBakers bs = return $ Map.keys $ bs ^. blockBirkParameters . birkActiveBakers . activeBakers

    {-# INLINE getAccountByCredId #-}
    getAccountByCredId bs cid =
      let mai = bs ^? blockAccounts . to Accounts.accountRegIds . ix cid
      in case mai of
           Nothing -> return Nothing
           Just ai -> return $ (ai, ) <$> bs ^? blockAccounts . Accounts.indexedAccount ai

    {-# INLINE getBakerAccount #-}
    getBakerAccount bs (BakerId ai) =
      return $ bs ^? blockAccounts . Accounts.indexedAccount ai

    {-# INLINE getModuleList #-}
    getModuleList bs = return $ bs ^. blockModules . to Modules.moduleRefList

    {-# INLINE getContractInstanceList #-}
    getContractInstanceList bs = return (bs ^.. blockInstances . Instances.foldInstances)

    {-# INLINE getAccountList #-}
    getAccountList bs =
      return $ AccountMap.addressesPure (Accounts.accountMap (bs ^. blockAccounts))

    getSeedState = return . view (blockBirkParameters . birkSeedState)

    getCurrentEpochBakers = return . epochToFullBakers . view (blockBirkParameters . birkCurrentEpochBakers . unhashed)

    getSlotBakers hbs genesisTime slotDuration slot = return $ case compare slotEpoch (epoch + 1) of
        -- LT should mean it's the current epoch, since the slot should be at least the slot of this block.
        LT -> epochToFullBakers (_unhashed (_birkCurrentEpochBakers bps))
        -- EQ means the next epoch.
        EQ -> epochToFullBakers (_unhashed (case _birkNextEpochBakers bps of
                UnchangedNextEpochBakers -> _birkCurrentEpochBakers bps
                NextEpochBakers neb -> neb
                ))
        -- GT means a future epoch, so consider everything in the active bakers,
        -- applying any adjustments that occur in an epoch < slotEpoch.
        GT -> FullBakers {
                fullBakerInfos = futureBakers,
                bakerTotalStake = sum (_bakerStake <$> futureBakers)
            }
      where
        bs = hbs ^. unhashedBlockState
        bps = bs ^. blockBirkParameters
        SeedState{..} = _birkSeedState bps
        slotEpoch = fromIntegral $ slot `quot` epochLength
        slotTime = addDuration genesisTime (fromIntegral slot * slotDuration)
        futureBakers = Vec.fromList $ foldr resolveBaker [] (Map.keys (_activeBakers (_birkActiveBakers bps)))
        resolveBaker (BakerId aid) l = case bs ^? blockAccounts . Accounts.indexedAccount aid of
            Just acct -> case acct ^. accountBaker of
              Just AccountBaker{..} -> case _bakerPendingChange of
                RemoveStake (PendingChangeEffectiveV0 remEpoch)
                  | remEpoch < slotEpoch -> l
                RemoveStake (PendingChangeEffectiveV1 remTime)
                  | remTime < slotTime -> l
                ReduceStake newAmt (PendingChangeEffectiveV0 redEpoch)
                  | redEpoch < slotEpoch -> (FullBakerInfo (_accountBakerInfo ^. bakerInfo) newAmt) : l
                ReduceStake newAmt (PendingChangeEffectiveV1 redTime)
                  | redTime < slotTime -> (FullBakerInfo (_accountBakerInfo ^. bakerInfo) newAmt) : l
                _ -> (FullBakerInfo (_accountBakerInfo ^. bakerInfo) _stakedAmount) : l
              Nothing -> error "Basic.getSlotBakers invariant violation: active baker account not a baker"
            Nothing -> error "Basic.getSlotBakers invariant violation: active baker account not valid"

    {-# INLINE getRewardStatus #-}
    getRewardStatus = return . view (blockBank . unhashed)

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

    {-# INLINE getProtocolUpdateStatus #-}
    getProtocolUpdateStatus bs = return (bs ^. blockUpdates . to protocolUpdateStatus)

    {-# INLINE getCryptographicParameters #-}
    getCryptographicParameters bs =
      return $! bs ^. blockCryptographicParameters . unhashed

instance (Monad m, IsProtocolVersion pv) => BS.AccountOperations (PureBlockStateMonad pv m) where

  getAccountCanonicalAddress acc = return $ acc ^. accountAddress

  getAccountAmount acc = return $ acc ^. accountAmount

  getAccountNonce acc = return $ acc ^. accountNonce

  checkAccountIsAllowed acc BS.AllowedEncryptedTransfers = return (Map.size (acc ^. accountCredentials) == 1)
  checkAccountIsAllowed acc BS.AllowedMultipleCredentials = return . isZeroAccountEncryptedAmount $ acc ^. accountEncryptedAmount

  getAccountCredentials acc = return $ acc ^. accountCredentials

  getAccountVerificationKeys acc = return $ acc ^. accountVerificationKeys

  getAccountEncryptedAmount acc = return $ acc ^. accountEncryptedAmount

  getAccountEncryptionKey acc = return $ acc ^. accountEncryptionKey

  getAccountReleaseSchedule acc = return $ acc ^. accountReleaseSchedule

  getAccountBaker acc = return $ acc ^. accountBaker

  getAccountBakerInfoRef acc = return $ (acc ^. accountBaker) <&> (^. bakerInfo)

  getAccountDelegator acc = return $ acc ^. accountDelegator

  getAccountStake acc = return $ acc ^. accountStaking

  derefBakerInfo = return

delegationConfigureDisallowOverdelegation
    :: (IsProtocolVersion pv, MTL.MonadError DelegationConfigureResult m)
    => BlockState pv
    -> PoolParameters 'ChainParametersV1
    -> DelegationTarget
    -> m ()
delegationConfigureDisallowOverdelegation bs poolParams target = case target of
  DelegateToLPool -> return ()
  DelegateToBaker bid@(BakerId baid) -> do
    let capitalBound = poolParams ^. ppCapitalBound
        leverageBound = poolParams ^. ppLeverageBound
        poolCapital = totalPoolCapital bs bid
    bakerEquityCapital <- case bs ^? blockAccounts . Accounts.indexedAccount baid of
      Just Account{_accountStaking = AccountStakeBaker ab} ->
          return (ab ^. stakedAmount)
      _ ->
          MTL.throwError (DCInvalidDelegationTarget bid)
    when (fromIntegral poolCapital > fromIntegral bakerEquityCapital * leverageBound) $
      MTL.throwError DCPoolStakeOverThreshold
    let epochBakers = bs ^. blockBirkParameters . birkCurrentEpochBakers . unhashed
        allCCD = _bakerTotalStake epochBakers
    when (poolCapital > takeFraction capitalBound allCCD) $
      MTL.throwError DCPoolOverDelegated

instance (IsProtocolVersion pv, Monad m) => BS.BlockStateOperations (PureBlockStateMonad pv m) where

    {-# INLINE bsoGetModule #-}
    bsoGetModule bs mref = return $ bs ^. blockModules . to (Modules.getInterface mref)

    {-# INLINE bsoGetInstance #-}
    bsoGetInstance bs caddr = return (Instances.getInstance caddr (bs ^. blockInstances))

    {-# INLINE bsoGetAccount #-}
    bsoGetAccount bs aaddr =
      return $ Accounts.getAccountWithIndex aaddr (bs ^. blockAccounts)

    {-# INLINE bsoGetAccountIndex #-}
    bsoGetAccountIndex bs aaddr = return $! Accounts.getAccountIndex aaddr (bs ^. blockAccounts)

    {-# INLINE bsoAddressWouldClash #-}
    bsoAddressWouldClash bs addr = return (Accounts.addressWouldClash addr (bs ^. blockAccounts))

    {-# INLINE bsoRegIdExists #-}
    bsoRegIdExists bs regid = return (Accounts.regIdExists regid (bs ^. blockAccounts))

    bsoCreateAccount bs gc addr cred = return $
            if Accounts.exists addr accounts then
              (Nothing, bs)
            else
              (Just acct, bs & blockAccounts .~ newAccounts)
        where
            acct = newAccount gc addr cred
            accounts = bs ^. blockAccounts
            newAccounts = Accounts.putAccountWithRegIds acct accounts

    bsoPutNewInstance bs mkInstance = return (instanceAddress, bs')
        where
            (inst, instances') = Instances.createInstance mkInstance (bs ^. blockInstances)
            Instances.InstanceParameters{..} = Instances.instanceParameters inst
            bs' = bs
                -- Add the instance
                & blockInstances .~ instances'

    bsoPutNewModule bs iface = return $!
        case Modules.putInterface iface (bs ^. blockModules) of
          Nothing -> (False, bs)
          Just mods' -> (True, bs & blockModules .~ mods')

    bsoModifyInstance bs caddr delta model = return $!
        bs & blockInstances %~ Instances.updateInstanceAt caddr delta model

    bsoModifyAccount bs accountUpdates = return $!
        -- Update the account
        bs & blockAccounts %~ Accounts.putAccount updatedAccount
        where
            account = bs ^. blockAccounts . singular (Accounts.indexedAccount (accountUpdates ^. auIndex))
            updatedAccount = Accounts.updateAccount accountUpdates account

    bsoSetAccountCredentialKeys bs accIndex credIx newKeys = return $! bs & blockAccounts %~ Accounts.putAccount updatedAccount
        where
            account = bs ^. blockAccounts . singular (Accounts.indexedAccount accIndex)
            updatedAccount = updateCredentialKeys credIx newKeys account

    bsoUpdateAccountCredentials bs accIndex remove add thrsh = return $! bs
            & blockAccounts %~ recordAllRegIds . updateAcct
        where
            updateAcct accts = Accounts.putAccountWithIndex updatedAccount accts
            recordAllRegIds (newIndex, newAccts) = Accounts.recordRegIds ((, newIndex) <$> credIdsToRecord) newAccts
            credIdsToRecord = Map.elems $ credId <$> add
            account = bs ^. blockAccounts . singular (Accounts.indexedAccount accIndex)
            updatedAccount = updateCredentials remove add thrsh account

    {-# INLINE bsoNotifyEncryptedBalanceChange #-}
    bsoNotifyEncryptedBalanceChange bs amntDiff =
      return $! bs & blockBank . unhashed . Rewards.totalEncryptedGTU %~ applyAmountDelta amntDiff

    {-# INLINE bsoGetSeedState #-}
    bsoGetSeedState bs = return $! bs ^. blockBirkParameters . birkSeedState

    {-# INLINE bsoSetSeedState #-}
    bsoSetSeedState bs ss = return $! bs & blockBirkParameters . birkSeedState .~ ss

    bsoRotateCurrentEpochBakers bs = return $! newbs
        where
            newbs = case bs ^. blockBirkParameters . birkNextEpochBakers of
                NextEpochBakers newCurrentBakers ->
                    bs & blockBirkParameters . birkCurrentEpochBakers .~ newCurrentBakers
                UnchangedNextEpochBakers -> bs

    bsoClearNextEpochBakers bs = return $! bs &
        blockBirkParameters . birkNextEpochBakers .~ UnchangedNextEpochBakers

    bsoSetNextEpochBakers bs bakers = return $! bs &
        blockBirkParameters . birkNextEpochBakers .~ NextEpochBakers newNextEpochBakers
        where
            newNextEpochBakers = makeHashed EpochBakers{..}
            bakers' = Vec.fromList bakers
            _bakerInfos = fst <$> bakers'
            _bakerStakes = snd <$> bakers'
            _bakerTotalStake = sum _bakerStakes

    -- This function handles removing bakers and delegators and reducing their stakes.
    bsoProcessPendingChanges oldBlockState isEffective = return $! newBlockState
        where
            newBlockState = MTL.execState processPendingChanges oldBlockState
            processPendingChanges = do
                -- Process the L-Pool
                oldDelegators <- use (blockBirkParameters . birkActiveBakers . lPoolDelegators)
                newDelegators <- processDelegators oldDelegators
                blockBirkParameters . birkActiveBakers . lPoolDelegators .= newDelegators
                -- Process the bakers (this may also modify the L-Pool)
                oldBakers <- use (blockBirkParameters . birkActiveBakers . activeBakers)
                newBakers <- processBakers oldBakers
                blockBirkParameters . birkActiveBakers . activeBakers .= newBakers

            -- For a set of delegators, process any pending changes on the account and return the
            -- new set of delegators. (A delegator is removed from the set if its pending change
            -- removes it.) This _only_ changes the accounts of delegators, and does not affect the
            -- active bakers record.
            processDelegators :: Set.Set DelegatorId -> MTL.State (BlockState pv) (Set.Set DelegatorId)
            processDelegators = fmap Set.fromDistinctAscList . filterM processDelegator . Set.toAscList
            processDelegator :: DelegatorId -> MTL.State (BlockState pv) Bool
            processDelegator (DelegatorId accId) =
                preuse (blockAccounts . Accounts.indexedAccount accId) >>= \case
                    Just acct -> updateAccountDelegator accId acct
                    Nothing -> error "Invariant violation: active delegator account was not found"
            updateAccountDelegator :: AccountIndex -> Account 'AccountV1 -> MTL.State (BlockState pv) Bool
            updateAccountDelegator accId acct = case acct ^. accountDelegator of
                Just acctDel@AccountDelegationV1{..} -> case _delegationPendingChange of
                    RemoveStake pet | isEffective pet -> removeDelegatorStake accId
                    ReduceStake newAmt pet | isEffective pet -> reduceDelegatorStake accId acctDel newAmt
                    _ -> return True
                Nothing -> error "Invariant violation: active delegator is not a delegation account"
            removeDelegatorStake :: AccountIndex -> MTL.State (BlockState pv) Bool
            removeDelegatorStake accId = do
                blockAccounts . Accounts.indexedAccount accId %=!
                    (accountStaking .~ AccountStakeNone)
                return False
            reduceDelegatorStake :: AccountIndex -> AccountDelegation 'AccountV1 -> Amount -> MTL.State (BlockState pv) Bool
            reduceDelegatorStake accId acctDel newAmt = do
                blockAccounts . Accounts.indexedAccount accId %=!
                    (accountStaking .~ AccountStakeDelegate acctDel{
                            _delegationStakedAmount = newAmt
                        })
                return True

            -- Process the bakers (this may also modify the L-Pool)
            processBakers :: Map.Map BakerId (Set.Set DelegatorId) -> MTL.State (BlockState pv) (Map.Map BakerId (Set.Set DelegatorId))
            processBakers = foldM processBaker Map.empty . Map.toAscList
            processBaker
                :: (Map.Map BakerId (Set.Set DelegatorId))
                -> (BakerId, Set.Set DelegatorId)
                -> MTL.State (BlockState pv) (Map.Map BakerId (Set.Set DelegatorId))
            processBaker accumBakers (bid@(BakerId accId), oldDelegators) = do
                newDelegators <- processDelegators oldDelegators
                preuse (blockAccounts . Accounts.indexedAccount accId) >>= \case
                    Just acct -> case acct ^. accountBaker of
                        Just acctBkr@AccountBaker{..} -> case _bakerPendingChange of
                            RemoveStake pet | isEffective pet ->
                                removeBaker accumBakers accId newDelegators
                            ReduceStake newAmt pet | isEffective pet ->
                                reduceBakerStake accumBakers bid newAmt acctBkr newDelegators
                            _ -> return $! Map.insert bid newDelegators accumBakers
                        Nothing -> error "Basic.bsoProcessPendingChanges invariant violation: active baker account not a baker"
                    Nothing -> error "Basic.bsoProcessPendingChanges invariant violation: active baker account not valid"
            removeBaker accumBakers accId delegators = do
                blockAccounts . Accounts.indexedAccount accId %=! (accountStaking .~ AccountStakeNone)
                forM_ delegators moveDelegationFromBaker
                blockBirkParameters . birkActiveBakers . lPoolDelegators %= Set.union delegators
                return $! accumBakers
            moveDelegationFromBaker (DelegatorId accId) =
                blockAccounts . Accounts.indexedAccount accId %=!
                    (accountStaking %~ \case
                        AccountStakeDelegate asd@AccountDelegationV1{} -> AccountStakeDelegate (asd{_delegationTarget = DelegateToLPool})
                        _ -> error "Invariant violation: active delegator is not a delegation account"
                        )
            reduceBakerStake accumBakers bid@(BakerId accId) newAmt acctBkr delegators = do
                let newAcctBkr = acctBkr{_stakedAmount = newAmt, _bakerPendingChange = NoChange}
                blockAccounts . Accounts.indexedAccount accId %=!
                    (accountStaking .~ AccountStakeBaker newAcctBkr)
                return $ Map.insert bid delegators accumBakers

    bsoTransitionEpochBakers bs newEpoch = return $! newbs
        where
            oldBPs = bs ^. blockBirkParameters
            curActiveBIDs = Map.toDescList (oldBPs ^. birkActiveBakers . activeBakers)
            -- Add a baker to the accumulated set of bakers for the new next bakers
            accumBakers (bs0, bkrs0) (bkr@(BakerId bid), _) = case bs ^? blockAccounts . Accounts.indexedAccount bid of
                Just acct -> case acct ^. accountBaker of
                  Just abkr@AccountBaker{..} -> case _bakerPendingChange of
                    RemoveStake (PendingChangeEffectiveV0 remEpoch)
                      -- The baker will be removed in the next epoch, so do not add it to the list
                      | remEpoch == newEpoch + 1 -> (bs0, bkrs0)
                      -- The baker is now removed, so do not add it to the list and remove it as an active baker
                      | remEpoch <= newEpoch -> (
                              bs0
                                -- remove baker id and aggregation key from active bakers
                                & blockBirkParameters . birkActiveBakers . activeBakers %~ Map.delete bkr
                                & blockBirkParameters . birkActiveBakers . aggregationKeys %~ Set.delete (_accountBakerInfo ^. bakerAggregationVerifyKey)
                                -- remove the account's baker record
                                & blockAccounts . Accounts.indexedAccount bid %~ (accountStaking .~ AccountStakeNone),
                              bkrs0
                              )
                    ReduceStake newAmt (PendingChangeEffectiveV0 redEpoch)
                      -- Reduction takes effect in the next epoch
                      | redEpoch == newEpoch + 1 -> (bs0, (abkr ^. bakerInfo, newAmt) : bkrs0)
                      -- Reduction is complete, so update the account accordingly.
                      | redEpoch <= newEpoch -> (
                              bs0
                                & blockAccounts . Accounts.indexedAccount bid %~
                                  (accountStaking .~ AccountStakeBaker abkr{_stakedAmount = newAmt, _bakerPendingChange = NoChange}),
                              (abkr ^. bakerInfo, newAmt) : bkrs0
                              )
                    _ -> (bs0, (abkr ^. bakerInfo, _stakedAmount) : bkrs0)
                  Nothing -> error "Basic.bsoTransitionEpochBakers invariant violation: active baker account not a baker"
                Nothing -> error "Basic.bsoTransitionEpochBakers invariant violation: active baker account not valid"
            (bs', bkrs) = foldl' accumBakers (bs, []) curActiveBIDs
            newNextBakers = makeHashed $ EpochBakers {
              _bakerInfos = Vec.fromList (fst <$> bkrs),
              _bakerStakes = Vec.fromList (snd <$> bkrs),
              _bakerTotalStake = foldl' (+) 0 (snd <$> bkrs)
            }
            newCurrentEpochBakers :: BasicBirkParameters 'AccountV0 -> Hashed EpochBakers
            newCurrentEpochBakers bp = case _birkNextEpochBakers bp of NextEpochBakers n -> n
            newbs = bs' & blockBirkParameters %~ \bp -> bp {
                        _birkCurrentEpochBakers = newCurrentEpochBakers bp,
                        _birkNextEpochBakers = NextEpochBakers newNextBakers
                    }

    bsoAddBaker bs ai BakerAdd{..} = do
      bakerStakeThreshold <- BS.bsoGetChainParameters bs <&> (^. cpPoolParameters . ppBakerStakeThreshold)
      return $! case bs ^? blockAccounts . Accounts.indexedAccount ai of
        -- Cannot resolve the account
        Nothing -> (BAInvalidAccount, bs)
        -- Account is already a baker
        Just Account{_accountStaking = AccountStakeBaker{}} -> (BAAlreadyBaker (BakerId ai), bs)
        Just Account{}
          -- Aggregation key is a duplicate
          | bkuAggregationKey baKeys `Set.member` (bs ^. blockBirkParameters . birkActiveBakers . aggregationKeys) -> (BADuplicateAggregationKey, bs)
          -- Provided stake is under threshold
          | baStake < bakerStakeThreshold -> (BAStakeUnderThreshold, bs)
          -- All checks pass, add the baker
          | otherwise -> let bid = BakerId ai in
              (BASuccess bid, bs
                & blockAccounts . Accounts.indexedAccount ai . accountStaking .~ AccountStakeBaker AccountBaker{
                  _stakedAmount = baStake,
                  _stakeEarnings = baStakeEarnings,
                  _accountBakerInfo = BakerInfoExV0 $ bakerKeyUpdateToInfo bid baKeys,
                  _bakerPendingChange = NoChange
                }
                & blockBirkParameters . birkActiveBakers . aggregationKeys %~ Set.insert (bkuAggregationKey baKeys)
                & blockBirkParameters . birkActiveBakers . activeBakers %~ Map.insert bid Set.empty
                )

    bsoConfigureBaker bs ai BakerConfigureAdd{..} = do
      -- It is assumed here that this account is NOT a baker and NOT a delegator.
      chainParams <- BS.bsoGetChainParameters bs
      let poolParams = chainParams ^. cpPoolParameters
      let capitalMin = poolParams ^. ppMinimumEquityCapital
      let ranges = poolParams ^. ppCommissionBounds
      return $! case bs ^? blockAccounts . Accounts.indexedAccount ai of
        -- Cannot resolve the account
        Nothing -> (BCInvalidAccount, bs)
        Just Account{}
          -- Aggregation key is a duplicate
          | bkuAggregationKey bcaKeys `Set.member` (bs ^. blockBirkParameters . birkActiveBakers . aggregationKeys) ->
                (BCDuplicateAggregationKey (bkuAggregationKey bcaKeys), bs)
          -- Provided stake is under threshold
          | bcaCapital < capitalMin ->
                (BCStakeUnderThreshold, bs)
          -- Check that commissions are within the valid ranges
          | not (isInRange bcaFinalizationRewardCommission (ranges ^. finalizationCommissionRange)) ->
                (BCCommissionNotInRange, bs)
          | not (isInRange bcaBakingRewardCommission (ranges ^. bakingCommissionRange)) ->
                (BCCommissionNotInRange, bs)
          | not (isInRange bcaTransactionFeeCommission (ranges ^. transactionCommissionRange)) ->
                (BCCommissionNotInRange, bs)
          -- All checks pass, add the baker
          | otherwise ->
            let bid = BakerId ai
                cr = CommissionRates {
                    _finalizationCommission = bcaFinalizationRewardCommission,
                    _bakingCommission = bcaBakingRewardCommission,
                    _transactionCommission = bcaTransactionFeeCommission
                }
                bpi = BakerPoolInfo {
                    _poolOpenStatus = bcaOpenForDelegation,
                    _poolMetadataUrl = bcaMetadataURL,
                    _poolCommissionRates = cr
                }
                bi = BakerInfoExV1 {
                    _bieBakerInfo = bakerKeyUpdateToInfo bid bcaKeys,
                    _bieBakerPoolInfo = bpi
                }
                ab = AccountStakeBaker AccountBaker{
                    _stakedAmount = bcaCapital,
                    _stakeEarnings = bcaRestakeEarnings,
                    _accountBakerInfo = bi,
                    _bakerPendingChange = NoChange
                }
                newBlockState = bs
                    & blockAccounts . Accounts.indexedAccount ai . accountStaking .~ ab
                    & blockBirkParameters . birkActiveBakers . aggregationKeys %~ Set.insert (bkuAggregationKey bcaKeys)
                    & blockBirkParameters . birkActiveBakers . activeBakers %~ Map.insert bid Set.empty
            in (BCSuccess [] bid, newBlockState)
    bsoConfigureBaker bs ai BakerConfigureRemove{..} = return $! case bs ^? blockAccounts . Accounts.indexedAccount ai of
        -- The account is valid and has a baker
        Just Account{_accountStaking = AccountStakeBaker ab@AccountBaker{..}}
          -- A change is already pending
          | _bakerPendingChange /= NoChange -> (BCChangePending, bs)
          -- We can make the change
          | otherwise ->
              let cp = bs ^. blockUpdates . currentParameters
                  rewardPeriodLength = fromIntegral $ cp ^. cpTimeParameters . tpRewardPeriodLength
                  cooldown = fromIntegral $ cp ^. cpCooldownParameters . cpPoolOwnerCooldown
                  msInEpoch = fromIntegral (epochLength $ bs ^. blockBirkParameters . birkSeedState) * bcrSlotDuration
                  timestamp = addDuration bcrSlotTimestamp (cooldown * rewardPeriodLength * msInEpoch)
              in (BCSuccess [] (BakerId ai),
                  bs & blockAccounts . Accounts.indexedAccount ai . accountStaking .~ AccountStakeBaker (ab & bakerPendingChange .~ RemoveStake (PendingChangeEffectiveV1 timestamp)))
        -- The account is not valid or has no baker
        _ -> (BCInvalidBaker, bs)
    bsoConfigureBaker origBS ai BakerConfigureUpdate{..} = do
        let res = MTL.runExcept $ MTL.runWriterT $ flip MTL.execStateT origBS $ do
                updateKeys
                updateRestakeEarnings
                updateOpenForDelegation
                updateMetadataURL
                updateTransactionFeeCommission
                updateBakingRewardCommission
                updateFinalizationRewardCommission
                updateCapital
        return $! case res of
            Left errorRes -> (errorRes, origBS)
            Right (newBS, changes) -> (BCSuccess changes bid, newBS)
      where
        bid = BakerId ai
        cooldownTimestamp =
            let cp = origBS ^. blockUpdates . currentParameters
                rewardPeriodLength = fromIntegral $ cp ^. cpTimeParameters . tpRewardPeriodLength
                cooldown = fromIntegral $ cp ^. cpCooldownParameters . cpPoolOwnerCooldown
                msInEpoch = fromIntegral (epochLength $ origBS ^. blockBirkParameters . birkSeedState) * bcuSlotDuration
            in addDuration bcuSlotTimestamp (cooldown * rewardPeriodLength * msInEpoch)
        getAccount = do
            s <- MTL.get
            case s ^? blockAccounts . Accounts.indexedAccount ai of
                Nothing -> MTL.throwError BCInvalidAccount
                Just Account{_accountStaking = AccountStakeBaker ab} -> return ab
                Just Account{} -> MTL.throwError BCInvalidBaker
        putAccount ab =
            MTL.modify' (blockAccounts . Accounts.indexedAccount ai . accountStaking .~ AccountStakeBaker ab)
        modifyAccount f = do
            ab <- getAccount
            putAccount $! f ab
        requireNoPendingChange = do
            ab <- getAccount
            when (_bakerPendingChange ab /= NoChange) (MTL.throwError BCChangePending)
        updateKeys = forM_ bcuKeys $ \keys -> do
            bs <- MTL.get
            ab <- getAccount
            let key = _bakerAggregationVerifyKey (ab ^. bakerInfo)
            let sameAgg = bkuAggregationKey keys == key
            let membAgg = Set.member
                            (bkuAggregationKey keys)
                            (bs ^. blockBirkParameters . birkActiveBakers . aggregationKeys)
            when (not sameAgg && membAgg) (MTL.throwError (BCDuplicateAggregationKey key))
            putAccount (ab & accountBakerInfo . bieBakerInfo .~ bakerKeyUpdateToInfo bid keys)
            MTL.modify'
                (blockBirkParameters . birkActiveBakers . aggregationKeys
                    %~ Set.insert (bkuAggregationKey keys) . Set.delete (ab ^. bakerAggregationVerifyKey))
            MTL.tell [BakerConfigureUpdateKeys keys]
        updateRestakeEarnings = forM_ bcuRestakeEarnings $ \restakeEarnings -> do
            ab <- getAccount
            unless (ab ^. stakeEarnings == restakeEarnings) $
              modifyAccount (stakeEarnings .~ restakeEarnings)
            MTL.tell [BakerConfigureRestakeEarnings restakeEarnings]
        updateOpenForDelegation = forM_ bcuOpenForDelegation $ \openForDelegation -> do
            ab <- getAccount
            unless (ab ^. accountBakerInfo . bieBakerPoolInfo . poolOpenStatus == openForDelegation) $
              modifyAccount (accountBakerInfo . bieBakerPoolInfo . poolOpenStatus .~ openForDelegation)
            MTL.tell [BakerConfigureOpenForDelegation openForDelegation]
        updateMetadataURL = forM_ bcuMetadataURL $ \metadataURL -> do
            ab <- getAccount
            unless (ab ^. accountBakerInfo . bieBakerPoolInfo . poolMetadataUrl == metadataURL) $
              modifyAccount (accountBakerInfo . bieBakerPoolInfo . poolMetadataUrl .~ metadataURL)
            MTL.tell [BakerConfigureMetadataURL metadataURL]
        updateTransactionFeeCommission = forM_ bcuTransactionFeeCommission $ \tfc -> do
            bs <- MTL.get
            let cp = bs ^. blockUpdates . currentParameters
            let range = cp ^. cpPoolParameters . ppCommissionBounds . transactionCommissionRange
            unless (isInRange tfc range) (MTL.throwError BCCommissionNotInRange)
            ab <- getAccount
            unless (ab ^. accountBakerInfo . bieBakerPoolInfo . poolCommissionRates . transactionCommission == tfc) $
              modifyAccount (accountBakerInfo . bieBakerPoolInfo . poolCommissionRates . transactionCommission .~ tfc)
            MTL.tell [BakerConfigureTransactionFeeCommission tfc]
        updateBakingRewardCommission = forM_ bcuBakingRewardCommission $ \brc -> do
            bs <- MTL.get
            let cp = bs ^. blockUpdates . currentParameters
            let range = cp ^. cpPoolParameters . ppCommissionBounds . bakingCommissionRange
            unless (isInRange brc range) (MTL.throwError BCCommissionNotInRange)
            ab <- getAccount
            unless (ab ^. accountBakerInfo . bieBakerPoolInfo . poolCommissionRates . bakingCommission == brc) $
              modifyAccount (accountBakerInfo . bieBakerPoolInfo . poolCommissionRates . bakingCommission .~ brc)
            MTL.tell [BakerConfigureBakingRewardCommission brc]
        updateFinalizationRewardCommission = forM_ bcuFinalizationRewardCommission $ \frc -> do
            bs <- MTL.get
            let cp = bs ^. blockUpdates . currentParameters
            let range = cp ^. cpPoolParameters . ppCommissionBounds . finalizationCommissionRange
            unless (isInRange frc range) (MTL.throwError BCCommissionNotInRange)
            ab <- getAccount
            unless (ab ^. accountBakerInfo . bieBakerPoolInfo . poolCommissionRates . finalizationCommission == frc) $
              modifyAccount (accountBakerInfo . bieBakerPoolInfo . poolCommissionRates . finalizationCommission .~ frc)
            MTL.tell [BakerConfigureFinalizationRewardCommission frc]
        updateCapital = forM_ bcuCapital $ \capital -> do
            requireNoPendingChange
            ab <- getAccount
            bs <- MTL.get
            let cp = bs ^. blockUpdates . currentParameters
            let capitalMin = cp ^. cpPoolParameters . ppMinimumEquityCapital
            when (capital < capitalMin) (MTL.throwError BCStakeUnderThreshold)
            case compare capital (_stakedAmount ab) of
                LT -> do
                    let bpc = ReduceStake capital (PendingChangeEffectiveV1 cooldownTimestamp)
                    modifyAccount (bakerPendingChange .~ bpc)
                    MTL.tell [BakerConfigureStakeReduced capital]
                EQ -> do
                    -- We could tell a "BakerConfigureStakeUnchanged", but currently it is not handled
                    -- in the Scheduler.
                    return ()
                GT -> do
                    modifyAccount (stakedAmount .~ capital)
                    MTL.tell [BakerConfigureStakeIncreased capital]

    bsoConfigureDelegation bs ai DelegationConfigureAdd{..} = do
        -- It is assumed here that this account is NOT a baker and NOT a delegator.
        poolParams <- _cpPoolParameters <$> BS.bsoGetChainParameters bs
        let result = MTL.runExcept $ do
                newBS <- updateBlockState
                delegationConfigureDisallowOverdelegation newBS poolParams dcaDelegationTarget
                return newBS
        return $! case result of
            Left e -> (e, bs)
            Right newBlockState -> (DCSuccess [] did, newBlockState)
        where
          did = DelegatorId ai
          updateBlockState = case bs ^? blockAccounts . Accounts.indexedAccount ai of
            Nothing -> MTL.throwError DCInvalidAccount
            Just Account{} -> do
              newBirkParams <- updateBirk dcaDelegationTarget
              let ad = AccountStakeDelegate AccountDelegationV1{
                      _delegationIdentity = did,
                      _delegationStakedAmount = dcaCapital,
                      _delegationStakeEarnings = dcaRestakeEarnings,
                      _delegationTarget = dcaDelegationTarget,
                      _delegationPendingChange = NoChange
                  }
                  newBlockState = bs
                    & blockAccounts . Accounts.indexedAccount ai . accountStaking .~ ad
                    & blockBirkParameters .~ newBirkParams
              return newBlockState
          updateBirk DelegateToLPool =
            return $! bs ^. blockBirkParameters
                & birkActiveBakers . lPoolDelegators %~ Set.insert did
          updateBirk (DelegateToBaker bid) =
            let ab = bs ^. blockBirkParameters . birkActiveBakers
                mDels = Map.lookup bid (ab ^. activeBakers)
            in case mDels of
                Nothing -> MTL.throwError (DCInvalidDelegationTarget bid)
                Just dels -> do
                    let newDels = Set.insert did dels
                        newActiveBakers = Map.insert bid newDels (ab ^. activeBakers)
                        newAB = ab{_activeBakers = newActiveBakers}
                    return $! _blockBirkParameters bs & birkActiveBakers .~ newAB
    bsoConfigureDelegation bs ai DelegationConfigureRemove{..} =
        return $! case bs ^? blockAccounts . Accounts.indexedAccount ai of
            Just Account{_accountStaking = AccountStakeDelegate ad@AccountDelegationV1{..}}
              | _delegationPendingChange /= NoChange -> (DCChangePending, bs)
              | otherwise ->
                  let cp = bs ^. blockUpdates . currentParameters
                      rewardPeriodLength = fromIntegral $ cp ^. cpTimeParameters . tpRewardPeriodLength
                      cooldown = fromIntegral $ cp ^. cpCooldownParameters . cpDelegatorCooldown
                      msInEpoch = fromIntegral (epochLength $ bs ^. blockBirkParameters . birkSeedState) * dcrSlotDuration
                      timestamp = addDuration dcrSlotTimestamp (cooldown * rewardPeriodLength * msInEpoch)
                      newBirkParams = updateBirk _delegationTarget
                  in (DCSuccess [] (DelegatorId ai),
                      bs
                        & blockAccounts . Accounts.indexedAccount ai . accountStaking .~ AccountStakeDelegate (ad & delegationPendingChange .~ RemoveStake (PendingChangeEffectiveV1 timestamp))
                        & blockBirkParameters .~ newBirkParams)
            _ -> (DCInvalidDelegator, bs)
        where
          updateBirk DelegateToLPool =
            bs ^. blockBirkParameters
                & birkActiveBakers . lPoolDelegators %~ Set.insert (DelegatorId ai)
          updateBirk (DelegateToBaker bid) =
            let ab = bs ^. blockBirkParameters . birkActiveBakers
                newBakers = Map.delete bid (ab ^. activeBakers)
                newAB = ab{_activeBakers = newBakers}
            in _blockBirkParameters bs & birkActiveBakers .~ newAB
    bsoConfigureDelegation origBS ai DelegationConfigureUpdate{..} = do
        poolParams <- _cpPoolParameters <$> BS.bsoGetChainParameters origBS
        let res = MTL.runExcept $ MTL.runWriterT $ flip MTL.execStateT origBS $ do
                updateDelegationTarget
                updateRestakeEarnings
                updateCapital poolParams
        return $! case res of
            Left errorRes -> (errorRes, origBS)
            Right (newBS, changes) -> (DCSuccess changes did, newBS)
      where
        did = DelegatorId ai
        cooldownTimestamp =
            let cp = origBS ^. blockUpdates . currentParameters
                rewardPeriodLength = fromIntegral $ cp ^. cpTimeParameters . tpRewardPeriodLength
                cooldown = fromIntegral $ cp ^. cpCooldownParameters . cpDelegatorCooldown
                msInEpoch = fromIntegral (epochLength $ origBS ^. blockBirkParameters . birkSeedState) * dcuSlotDuration
            in addDuration dcuSlotTimestamp (cooldown * rewardPeriodLength * msInEpoch)
        getAccount = do
            s <- MTL.get
            case s ^? blockAccounts . Accounts.indexedAccount ai of
                Nothing -> MTL.throwError DCInvalidAccount
                Just Account{_accountStaking = AccountStakeDelegate ad} -> return ad
                Just Account{} -> MTL.throwError DCInvalidDelegator
        putAccount ad =
            MTL.modify' (blockAccounts . Accounts.indexedAccount ai . accountStaking .~ AccountStakeDelegate ad)
        modifyAccount f = do
            ad <- getAccount
            putAccount $! f ad
        requireNoPendingChange = do
            ad <- getAccount
            when (_delegationPendingChange ad /= NoChange) (MTL.throwError DCChangePending)
        updateCapital poolParams = forM_ dcuCapital $ \capital -> do
            requireNoPendingChange
            ad <- getAccount
            case compare capital (ad ^. delegationStakedAmount) of
                LT -> do
                    let dpc = ReduceStake capital (PendingChangeEffectiveV1 cooldownTimestamp)
                    modifyAccount (delegationPendingChange .~ dpc)
                    MTL.tell [DelegationConfigureStakeReduced capital]
                EQ ->
                    -- We could tell "DelegationConfigureStakeUnchanged", but currently it is not
                    -- handled in the Scheduler.
                    return ()
                GT -> do
                    modifyAccount (delegationStakedAmount .~ capital)
                    bs <- MTL.get
                    -- The delegation target may be updated by 'updateDelegationTarget', hence it
                    -- is important that 'updateDelegationTarget' is invoked before 'updateCapital'.
                    delegationConfigureDisallowOverdelegation bs poolParams (ad ^. delegationTarget)
                    MTL.tell [DelegationConfigureStakeIncreased capital]
        updateRestakeEarnings = forM_ dcuRestakeEarnings $ \restakeEarnings -> do
            ad <- getAccount
            unless (restakeEarnings == ad ^. delegationStakeEarnings) $
              modifyAccount $ delegationStakeEarnings .~ restakeEarnings
            MTL.tell [DelegationConfigureRestakeEarnings restakeEarnings]
        updateDelegationTarget = forM_ dcuDelegationTarget $ \target -> do
            ad <- getAccount
            unless (target == ad ^. delegationTarget) $
              modifyAccount (delegationTarget .~ target)
            MTL.tell [DelegationConfigureDelegationTarget target]

    bsoUpdateBakerKeys bs ai bku@BakerKeyUpdate{..} = return $! case bs ^? blockAccounts . Accounts.indexedAccount ai of
        -- The account is valid and has a baker
        Just Account{_accountStaking = AccountStakeBaker ab}
          -- The key would duplicate an existing aggregation key (other than the baker's current key)
          | bkuAggregationKey /= _bakerAggregationVerifyKey (ab ^. bakerInfo)
          , bkuAggregationKey `Set.member` (bs ^. blockBirkParameters . birkActiveBakers . aggregationKeys) -> (BKUDuplicateAggregationKey, bs)
          -- The aggregation key is not a duplicate, so update the baker
          | otherwise -> (BKUSuccess (BakerId ai), bs
              & blockAccounts . Accounts.indexedAccount ai . accountStaking .~
                AccountStakeBaker ab{_accountBakerInfo = BakerInfoExV0 $ bakerKeyUpdateToInfo (ab ^. bakerIdentity) bku}
              & blockBirkParameters . birkActiveBakers . aggregationKeys %~ Set.insert bkuAggregationKey . Set.delete (ab ^. bakerAggregationVerifyKey)
              )
        -- Cannot resolve the account, or it is not a baker
        _ -> (BKUInvalidBaker, bs)

    bsoUpdateBakerStake bs ai newStake = do
      bakerStakeThreshold <- BS.bsoGetChainParameters bs <&> (^. cpPoolParameters . ppBakerStakeThreshold)
      return $! case bs ^? blockAccounts . Accounts.indexedAccount ai of
        -- The account is valid and has a baker
        Just Account{_accountStaking = AccountStakeBaker ab@AccountBaker{..}}
          -- A change is already pending
          | _bakerPendingChange /= NoChange -> (BSUChangePending (BakerId ai), bs)
          -- We can make the change
          | otherwise ->
              let mres = case compare newStake _stakedAmount of
                          LT -> let curEpoch = epoch $ bs ^. blockBirkParameters . birkSeedState
                                    cooldown = 2 + bs ^. blockUpdates . currentParameters . cpCooldownParameters . cpBakerExtraCooldownEpochs
                                in
                                  if newStake < bakerStakeThreshold
                                  then Left BSUStakeUnderThreshold
                                  else Right (BSUStakeReduced (BakerId ai) (curEpoch + cooldown), bakerPendingChange .~ ReduceStake newStake (PendingChangeEffectiveV0 $ curEpoch + cooldown))
                          EQ -> Right (BSUStakeUnchanged (BakerId ai), id)
                          GT -> Right (BSUStakeIncreased (BakerId ai), stakedAmount .~ newStake)
              in case mres of
                Right (res, updateStake) -> (res, bs & blockAccounts . Accounts.indexedAccount ai . accountStaking .~ AccountStakeBaker (ab & updateStake))
                Left e -> (e, bs)
        -- The account is not valid or has no baker
        _ -> (BSUInvalidBaker, bs)

    bsoUpdateBakerRestakeEarnings bs ai newRestakeEarnings = return $! case bs ^? blockAccounts . Accounts.indexedAccount ai of
        -- The account is valid and has a baker
        Just Account{_accountStaking = AccountStakeBaker ab@AccountBaker{..}} ->
          if newRestakeEarnings == _stakeEarnings
          -- No actual change
          then (BREUUpdated (BakerId ai), bs)
          -- A real change
          else (BREUUpdated (BakerId ai), bs & blockAccounts . Accounts.indexedAccount ai . accountStaking .~ AccountStakeBaker ab{_stakeEarnings = newRestakeEarnings})
        _ -> (BREUInvalidBaker, bs)

    bsoRemoveBaker bs ai = return $! case bs ^? blockAccounts . Accounts.indexedAccount ai of
        -- The account is valid and has a baker
        Just Account{_accountStaking = AccountStakeBaker ab@AccountBaker{..}}
          -- A change is already pending
          | _bakerPendingChange /= NoChange -> (BRChangePending (BakerId ai), bs)
          -- We can make the change
          | otherwise ->
              let curEpoch = epoch $ bs ^. blockBirkParameters . birkSeedState
                  cooldown = 2 + bs ^. blockUpdates . currentParameters . cpCooldownParameters . cpBakerExtraCooldownEpochs
              in (BRRemoved (BakerId ai) (curEpoch + cooldown),
                  bs & blockAccounts . Accounts.indexedAccount ai . accountStaking .~ AccountStakeBaker (ab & bakerPendingChange .~ RemoveStake (PendingChangeEffectiveV0 $ curEpoch + cooldown)))
        -- The account is not valid or has no baker
        _ -> (BRInvalidBaker, bs)

    -- This uses that baker identities are account indexes.  The account with the corresponding
    -- index (if any) is given the reward.  If the account has a baker (which it presumably should) then
    -- the stake is increased correspondingly if 'stakeEarnings' is set.
    bsoRewardBaker bs (BakerId ai) !reward = return (getFirst <$> mfaddr, bs')
      where
        (mfaddr, !bs') = bs & (blockAccounts . Accounts.indexedAccount ai) payReward
        payReward acct = (Just . First $! acct ^. accountAddress, acct & accountAmount +~ reward & accountStaking %~ updateBaker)
        updateBaker (AccountStakeBaker bkr)
          | _stakeEarnings bkr = AccountStakeBaker $ bkr & stakedAmount +~ reward
        updateBaker stk = stk

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
    bsoProcessUpdateQueues bs ts = return (changes, bs & blockUpdates .~ newBlockUpdates
                                                       & blockAnonymityRevokers .~ makeHashed updatedARs
                                                       & blockIdentityProviders .~ makeHashed updatedIPs)
      where
        (u, ars, ips) = (bs ^. blockUpdates, bs ^. blockAnonymityRevokers . unhashed, bs ^. blockIdentityProviders . unhashed)
        (!changes, !(newBlockUpdates, updatedARs, updatedIPs)) = processUpdateQueues ts (u, ars, ips)

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


    {-# INLINE bsoGetUpdateKeyCollection #-}
    bsoGetUpdateKeyCollection bs = return $! bs ^. blockUpdates . currentKeyCollection . unhashed

    {-# INLINE bsoGetNextUpdateSequenceNumber #-}
    bsoGetNextUpdateSequenceNumber bs uty = return $! lookupNextUpdateSequenceNumber (bs ^. blockUpdates) uty

    {-# INLINE bsoEnqueueUpdate #-}
    bsoEnqueueUpdate bs effectiveTime payload = return $! bs & blockUpdates %~ enqueueUpdate effectiveTime payload

    {-# INLINE bsoOverwriteElectionDifficulty #-}
    bsoOverwriteElectionDifficulty bs newDifficulty = return $! bs & blockUpdates %~ overwriteElectionDifficulty newDifficulty

    {-# INLINE bsoClearProtocolUpdate #-}
    bsoClearProtocolUpdate bs = return $! bs & blockUpdates %~ clearProtocolUpdate

    {-# INLINE bsoAddReleaseSchedule #-}
    bsoAddReleaseSchedule bs rel = do
      let f relSchedule (addr, t) = Map.alter (\case
                                                  Nothing -> Just t
                                                  Just t' -> Just $ min t' t) addr relSchedule
          updateBRS brs = foldl' f brs rel
      return $! bs & blockReleaseSchedule %~ updateBRS

    {-# INLINE bsoGetEnergyRate #-}
    bsoGetEnergyRate bs = return $! bs ^. blockUpdates . currentParameters . energyRate

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

instance (IsProtocolVersion pv, MonadIO m) => BS.BlockStateStorage (PureBlockStateMonad pv m) where
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

    {-# INLINE serializeBlockState #-}
    serializeBlockState = return . runPut . putBlockState . _unhashedBlockState

    {-# INLINE writeBlockState #-}
    writeBlockState h = PureBlockStateMonad . liftIO . hPutBuilder h . snd . runPutMBuilder . putBlockState . _unhashedBlockState

-- |Initial block state.
initialState :: (IsProtocolVersion pv)
             => SeedState
             -> CryptographicParameters
             -> [Account (AccountVersionFor pv)]
             -> IPS.IdentityProviders
             -> ARS.AnonymityRevokers
             -> UpdateKeysCollection (ChainParametersVersionFor pv)
             -> ChainParameters pv
             -> BlockState pv
initialState seedState cryptoParams genesisAccounts ips anonymityRevokers keysCollection chainParams = BlockState {..}
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
    _blockUpdates = initialUpdates keysCollection chainParams
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

-- |Initial block state based on 'GenesisData', for a given protocol version.
genesisState :: forall pv . IsProtocolVersion pv => GenesisData pv -> Either String (BlockState pv)
genesisState gd = case protocolVersion @pv of
                    SP1 -> case gd of
                      GDP1 P1.GDP1Initial{..} -> mkGenesisStateInitial genesisCore genesisInitialState
                      GDP1 P1.GDP1Regenesis{..} -> mkGenesisStateRegenesis StateMigrationParametersTrivial genesisRegenesis
                    SP2 -> case gd of
                      GDP2 P2.GDP2Initial{..} -> mkGenesisStateInitial genesisCore genesisInitialState
                      GDP2 P2.GDP2Regenesis{..} -> mkGenesisStateRegenesis StateMigrationParametersTrivial genesisRegenesis
                    SP3 -> case gd of
                      GDP3 P3.GDP3Initial{..} -> mkGenesisStateInitial genesisCore genesisInitialState
                      GDP3 P3.GDP3Regenesis{..} -> mkGenesisStateRegenesis StateMigrationParametersTrivial genesisRegenesis
    where
        mkGenesisStateInitial GenesisData.CoreGenesisParameters{..} GenesisData.GenesisState{..} = do
            accounts <- mapM mkAccount (zip [0..] (toList genesisAccounts))
            let
                _blockBirkParameters = initialBirkParameters accounts genesisSeedState
                _blockAccounts = List.foldl' (flip Accounts.putAccountWithRegIds) Accounts.emptyAccounts accounts
                -- initial amount in the central bank is the amount on all genesis accounts combined
                initialAmount = List.foldl' (\c acc -> c + acc ^. accountAmount) 0 accounts
                _blockBank = makeHashed $ Rewards.makeGenesisBankStatus initialAmount
            return BlockState {..}
              where
                  mkAccount (bid, GenesisAccount{..}) =
                      case gaBaker of
                        Just GenesisBaker{..} | gbBakerId /= bid -> Left "Mismatch between assigned and chosen baker id."
                        _ -> Right $! newAccountMultiCredential genesisCryptographicParameters gaThreshold gaAddress gaCredentials
                                  & accountAmount .~ gaBalance
                                  & case gaBaker of
                                      Nothing -> id
                                      Just GenesisBaker{..} -> accountStaking .~ AccountStakeBaker AccountBaker {
                                        _stakedAmount = gbStake,
                                        _stakeEarnings = gbRestakeEarnings,
                                        _accountBakerInfo = BakerInfoExV0 BakerInfo {
                                            _bakerIdentity = bid,
                                            _bakerSignatureVerifyKey = gbSignatureVerifyKey,
                                            _bakerElectionVerifyKey = gbElectionVerifyKey,
                                            _bakerAggregationVerifyKey = gbAggregationVerifyKey
                                            },
                                        _bakerPendingChange = NoChange
                                        }
                  genesisSeedState = initialSeedState genesisLeadershipElectionNonce genesisEpochLength
                  _blockCryptographicParameters = makeHashed genesisCryptographicParameters
                  _blockInstances = Instances.emptyInstances
                  _blockModules = Modules.emptyModules
                  _blockIdentityProviders = makeHashed genesisIdentityProviders
                  _blockAnonymityRevokers = makeHashed genesisAnonymityRevokers
                  _blockTransactionOutcomes = Transactions.emptyTransactionOutcomes
                  _blockUpdates = initialUpdates genesisUpdateKeys genesisChainParameters
                  _blockReleaseSchedule = Map.empty
                  _blockEpochBlocksBaked = emptyHashedEpochBlocks

        mkGenesisStateRegenesis migration GenesisData.RegenesisData{..} = do
            case runGet (getBlockState migration) genesisNewState of
                Left err -> Left $ "Could not deserialize genesis state: " ++ err
                Right bs
                    | hbs ^. blockStateHash /= genesisStateHash -> Left "Could not deserialize genesis state: state hash is incorrect"
                    | epochLength (bs ^. blockBirkParameters . birkSeedState) /= GenesisData.genesisEpochLength genesisCore -> Left "Could not deserialize genesis state: epoch length mismatch"
                    | otherwise -> Right bs
                    where
                        hbs = hashBlockState bs
