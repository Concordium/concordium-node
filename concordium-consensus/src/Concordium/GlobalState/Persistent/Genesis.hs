{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |This module contains functions to construct an initial block state from genesis data.
module Concordium.GlobalState.Persistent.Genesis (genesisState) where

import qualified Concordium.Genesis.Data as GenesisData
import qualified Concordium.Genesis.Data.BaseV1 as GDBaseV1
import qualified Concordium.Genesis.Data.P1 as P1
import qualified Concordium.Genesis.Data.P2 as P2
import qualified Concordium.Genesis.Data.P3 as P3
import qualified Concordium.Genesis.Data.P4 as P4
import qualified Concordium.Genesis.Data.P5 as P5
import qualified Concordium.Genesis.Data.P6 as P6
import qualified Concordium.GlobalState.Basic.BlockState.PoolRewards as Basic
import qualified Concordium.GlobalState.CapitalDistribution as CapDist
import qualified Concordium.GlobalState.Persistent.Account as Account
import qualified Concordium.GlobalState.Persistent.Accounts as Accounts
import qualified Concordium.GlobalState.Persistent.Bakers as Bakers
import qualified Concordium.GlobalState.Persistent.BlobStore as Blob
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import qualified Concordium.GlobalState.Persistent.BlockState.Modules as Modules
import qualified Concordium.GlobalState.Persistent.BlockState.Updates as Updates
import qualified Concordium.GlobalState.Persistent.Instances as Instances
import qualified Concordium.GlobalState.Persistent.LFMBTree as LFMBT
import qualified Concordium.GlobalState.Persistent.PoolRewards as Rewards
import qualified Concordium.GlobalState.Persistent.ReleaseSchedule as ReleaseSchedule
import qualified Concordium.GlobalState.Persistent.Trie as Trie
import qualified Concordium.GlobalState.Rewards as Rewards
import qualified Concordium.GlobalState.TransactionTable as TransactionTable
import qualified Concordium.Types as Types
import qualified Concordium.Types.Parameters as Types
import qualified Concordium.Types.SeedState as Types

import qualified Control.Monad.Except as MTL
import Data.IORef (newIORef)
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Vector as Vec
import Lens.Micro.Platform

----------- API -----------

-- |Initial block state based on 'GenesisData', for a given protocol version.
-- This also returns the transaction table.
-- The result is immediately flushed to disc and cached.
genesisState ::
    forall pv av m.
    (BS.SupportsPersistentState pv m, Types.AccountVersionFor pv ~ av) =>
    GenesisData.GenesisData pv ->
    m (Either String (BS.HashedPersistentBlockState pv, TransactionTable.TransactionTable))
genesisState gd = MTL.runExceptT $ case Types.protocolVersion @pv of
    Types.SP1 -> case gd of
        GenesisData.GDP1 P1.GDP1Initial{..} ->
            buildGenesisBlockState (CGPV0 genesisCore) genesisInitialState
    Types.SP2 -> case gd of
        GenesisData.GDP2 P2.GDP2Initial{..} ->
            buildGenesisBlockState (CGPV0 genesisCore) genesisInitialState
    Types.SP3 -> case gd of
        GenesisData.GDP3 P3.GDP3Initial{..} ->
            buildGenesisBlockState (CGPV0 genesisCore) genesisInitialState
    Types.SP4 -> case gd of
        GenesisData.GDP4 P4.GDP4Initial{..} ->
            buildGenesisBlockState (CGPV0 genesisCore) genesisInitialState
    Types.SP5 -> case gd of
        GenesisData.GDP5 P5.GDP5Initial{..} ->
            buildGenesisBlockState (CGPV0 genesisCore) genesisInitialState
    Types.SP6 -> case gd of
        GenesisData.GDP6 P6.GDP6Initial{..} ->
            buildGenesisBlockState (CGPV1 genesisCore) genesisInitialState

-------- Types -----------

-- |A GADT that wraps the core genesis parameters for each consensus version.
data VersionedCoreGenesisParameters (pv :: Types.ProtocolVersion) where
    CGPV0 :: (Types.IsConsensusV0 pv) => GenesisData.CoreGenesisParameters -> VersionedCoreGenesisParameters pv
    CGPV1 :: (Types.IsConsensusV1 pv) => GDBaseV1.CoreGenesisParametersV1 -> VersionedCoreGenesisParameters pv

-- |State being accumulated while iterating the accounts in genesis data.
-- It is then used to construct the initial block state from genesis.
data AccumGenesisState pv = AccumGenesisState
    { -- | Tracking all the accounts.
      agsAllAccounts :: !(Accounts.Accounts pv),
      -- | Collection of the IDs of the active bakers.
      agsBakerIds :: !(Bakers.BakerIdTrieMap (Types.AccountVersionFor pv)),
      -- | Collection of the aggregation keys of the active bakers.
      agsBakerKeys :: !Bakers.AggregationKeySet,
      -- | Total amount owned by accounts.
      agsTotal :: !Types.Amount,
      -- | Total staked amount by bakers.
      agsStakedTotal :: !Types.Amount,
      -- | List of baker info refs in incremental order of the baker ID.
      agsBakerInfoRefs :: !(Vec.Vector (Account.PersistentBakerInfoRef (Types.AccountVersionFor pv))),
      -- | List of baker stake in incremental order of the baker ID.
      -- Entries in this list should have a matching entry in agsBakerCapitals.
      -- In the end result these are needed separately and are therefore constructed separately.
      agsBakerStakes :: !(Vec.Vector Types.Amount),
      -- | List of baker capital in incremental order of the baker ID.
      -- Entries in this list should have a matching entry in agsBakerStakes.
      -- In the end result these are needed separately and are therefore constructed separately.
      agsBakerCapitals :: !(Vec.Vector CapDist.BakerCapital)
    }

--------- Helper functions ----------

-- | The initial value for accumulating data from genesis data accounts.
initialAccumGenesisState :: AccumGenesisState pv
initialAccumGenesisState =
    AccumGenesisState
        { agsAllAccounts = Accounts.emptyAccounts,
          agsBakerIds = Trie.empty,
          agsBakerKeys = Trie.empty,
          agsTotal = 0,
          agsStakedTotal = 0,
          agsBakerInfoRefs = Vec.empty,
          agsBakerStakes = Vec.empty,
          agsBakerCapitals = Vec.empty
        }

-- | Construct a hashed persistent block state from the data in genesis.
-- The result is immediately flushed to disc and cached.
buildGenesisBlockState ::
    forall pv av m.
    (BS.SupportsPersistentState pv m, Types.AccountVersionFor pv ~ av) =>
    VersionedCoreGenesisParameters pv ->
    GenesisData.GenesisState pv ->
    MTL.ExceptT String m (BS.HashedPersistentBlockState pv, TransactionTable.TransactionTable)
buildGenesisBlockState vcgp GenesisData.GenesisState{..} = do
    -- Iterate the accounts in genesis once and accumulate all relevant information.
    AccumGenesisState{..} <- Vec.ifoldM' accumStateFromGenesisAccounts initialAccumGenesisState genesisAccounts

    -- Birk parameters
    persistentBirkParameters :: BS.PersistentBirkParameters pv <- do
        _birkActiveBakers <-
            Blob.refMakeFlushed $
                Bakers.PersistentActiveBakers
                    { _activeBakers = agsBakerIds,
                      _aggregationKeys = agsBakerKeys,
                      _passiveDelegators = Bakers.emptyPersistentActiveDelegators,
                      _totalActiveCapital = case Types.delegationSupport @av of
                        Types.SAVDelegationNotSupported -> Bakers.TotalActiveCapitalV0
                        Types.SAVDelegationSupported -> Bakers.TotalActiveCapitalV1 agsTotal
                    }

        _birkNextEpochBakers <-
            Blob.refMakeFlushed =<< do
                _bakerInfos <- Blob.refMakeFlushed $ Bakers.BakerInfos agsBakerInfoRefs
                _bakerStakes <- Blob.refMakeFlushed $ Bakers.BakerStakes agsBakerStakes
                return Bakers.PersistentEpochBakers{_bakerTotalStake = agsStakedTotal, ..}

        let _birkSeedState = case vcgp of
                CGPV0 GenesisData.CoreGenesisParameters{..} -> Types.initialSeedStateV0 genesisLeadershipElectionNonce genesisEpochLength
                CGPV1 _ -> Types.initialSeedStateV1 genesisLeadershipElectionNonce

        return $
            BS.PersistentBirkParameters
                { _birkCurrentEpochBakers = _birkNextEpochBakers,
                  ..
                }

    -- Reward details
    rewardDetails <- case Types.delegationSupport @av of
        Types.SAVDelegationNotSupported ->
            return $ BS.BlockRewardDetailsV0 BS.emptyHashedEpochBlocks
        Types.SAVDelegationSupported ->
            case Types.delegationChainParameters @pv of
                Types.DelegationChainParameters -> do
                    capRef :: Blob.HashedBufferedRef CapDist.CapitalDistribution <-
                        Blob.refMakeFlushed
                            CapDist.CapitalDistribution
                                { bakerPoolCapital = agsBakerCapitals,
                                  passiveDelegatorsCapital = Vec.empty
                                }
                    bakerPoolRewardDetails <-
                        LFMBT.fromAscList $
                            replicate (Vec.length agsBakerCapitals) Basic.emptyBakerPoolRewardDetails
                    BS.BlockRewardDetailsV1
                        <$> Blob.refMakeFlushed
                            Rewards.PoolRewards
                                { nextCapital = capRef,
                                  currentCapital = capRef,
                                  bakerPoolRewardDetails = bakerPoolRewardDetails,
                                  passiveDelegationTransactionRewards = 0,
                                  foundationTransactionRewards = 0,
                                  nextPaydayEpoch =
                                    genesisChainParameters
                                        ^. Types.cpTimeParameters
                                            . Types.tpRewardPeriodLength
                                            . to Types.rewardPeriodEpochs,
                                  nextPaydayMintRate =
                                    genesisChainParameters
                                        ^. Types.cpTimeParameters
                                            . Types.tpMintPerPayday
                                }

    -- Module
    modules <- Blob.refMakeFlushed Modules.emptyModules

    -- Identity providers and anonymity revokers
    identityProviders <- Blob.bufferHashed $ Types.makeHashed genesisIdentityProviders
    anonymityRevokers <- Blob.bufferHashed $ Types.makeHashed genesisAnonymityRevokers

    cryptographicParameters <- Blob.bufferHashed $ Types.makeHashed genesisCryptographicParameters

    persistentUpdates <- Updates.initialUpdates genesisUpdateKeys genesisChainParameters
    updates <- Blob.refMakeFlushed persistentUpdates

    releaseSchedule <- ReleaseSchedule.emptyReleaseSchedule
    bsp <-
        Blob.refMakeFlushed $
            BS.BlockStatePointers
                { bspAccounts = agsAllAccounts,
                  bspInstances = Instances.emptyInstances,
                  bspModules = modules,
                  bspBank = Types.makeHashed $ Rewards.makeGenesisBankStatus agsTotal,
                  bspIdentityProviders = identityProviders,
                  bspAnonymityRevokers = anonymityRevokers,
                  bspBirkParameters = persistentBirkParameters,
                  bspCryptographicParameters = cryptographicParameters,
                  bspTransactionOutcomes = BS.emptyPersistentTransactionOutcomes,
                  bspUpdates = updates,
                  bspReleaseSchedule = releaseSchedule,
                  bspRewardDetails = rewardDetails
                }
    bps <- MTL.liftIO $ newIORef $! bsp
    hashedBlockState <- BS.hashBlockState bps
    return (hashedBlockState, TransactionTable.emptyTransactionTable)
  where
    -- For iterating the genesis accounts and accumulating relevant states to build up the genesis block.
    accumStateFromGenesisAccounts ::
        -- The state being accumulated so far.
        AccumGenesisState pv ->
        -- The index of the account
        Int ->
        -- Account from genesis to accumulate.
        GenesisData.GenesisAccount ->
        MTL.ExceptT String m (AccumGenesisState pv)
    accumStateFromGenesisAccounts state index genesisAccount = do
        -- Create the persistent account
        !persistentAccount <-
            Account.makeFromGenesisAccount
                (Types.protocolVersion @pv)
                genesisCryptographicParameters
                genesisChainParameters
                genesisAccount
        -- Insert the account
        (maybeIndex, nextAccounts) <- Accounts.putNewAccount persistentAccount $ agsAllAccounts state
        MTL.when (isNothing maybeIndex) $
            MTL.throwError "Duplicate account address in genesis accounts."

        let !nextTotalAmount = agsTotal state + GenesisData.gaBalance genesisAccount
        let !updatedState = state{agsAllAccounts = nextAccounts, agsTotal = nextTotalAmount}

        case GenesisData.gaBaker genesisAccount of
            Just baker@GenesisData.GenesisBaker{..} -> do
                MTL.unless (gbBakerId == fromIntegral index) $
                    MTL.throwError "Mismatch between assigned and chosen baker id."

                let !nextStakedTotal = agsStakedTotal state + gbStake
                nextBakerIds <-
                    Trie.insert gbBakerId Bakers.emptyPersistentActiveDelegators $
                        agsBakerIds state
                nextBakerKeys <- Trie.insert gbAggregationVerifyKey () $ agsBakerKeys state
                let !nextBakerStakes = Vec.snoc (agsBakerStakes state) gbStake

                infoRef <-
                    -- If the result is Nothing, we have failed to construct the account as a baker
                    -- which is a bug in the code, hence the usage of error and not throwError.
                    fromMaybe (error "Invariant violation: genesis baker is not baker")
                        <$> Account.accountBakerInfoRef persistentAccount
                let !nextBakerInfoRefs = Vec.snoc (agsBakerInfoRefs state) infoRef
                let !nextBakerCapitals = Vec.snoc (agsBakerCapitals state) $ bakerCapitalFromGenesis baker
                return $!
                    updatedState
                        { agsBakerIds = nextBakerIds,
                          agsBakerKeys = nextBakerKeys,
                          agsStakedTotal = nextStakedTotal,
                          agsBakerInfoRefs = nextBakerInfoRefs,
                          agsBakerStakes = nextBakerStakes,
                          agsBakerCapitals = nextBakerCapitals
                        }
            Nothing -> return updatedState

-- |Construct baker capital from genesis baker.
bakerCapitalFromGenesis :: GenesisData.GenesisBaker -> CapDist.BakerCapital
bakerCapitalFromGenesis GenesisData.GenesisBaker{..} =
    CapDist.BakerCapital
        { bcBakerId = gbBakerId,
          bcBakerEquityCapital = gbStake,
          bcDelegatorCapital = Vec.empty
        }
