{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Node-owned persistent chain parameters.
--
-- This type is the persistent node representation used by the update state. It
-- is distinct from the @concordium-base@ public/wire @ChainParameters'@ view.
-- The aggregate public/wire type is only used at conversion boundaries; the
-- persistent storage model has its own record fields and a P11-and-onwards
-- Rust-managed external chain-parameters pointer.
module Concordium.GlobalState.Persistent.BlockState.Parameters (
    PersistentChainParameters,
    PersistentChainParameters' (..),
    makePersistentChainParameters,
    persistentChainParametersToChainParameters,
    persistentChainParametersToChainParametersM,
    updateChainParameters,
) where

import Control.Monad.IO.Class
import Data.Bool.Singletons
import qualified Data.ByteString as BS
import qualified Data.Serialize as S
import Data.Singletons

import qualified Concordium.Crypto.SHA256 as H
import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.GlobalState.Persistent.BlockState.ExternalChainParameters as ECP
import Concordium.Types
import Concordium.Types.Conditionally
import Concordium.Types.HashableTo
import Concordium.Types.Parameters

-- | Persistent node-owned chain parameters.
data PersistentChainParameters' cpv auv = PersistentChainParameters
    { -- | Consensus parameters.
      pcpConsensusParameters :: !(ConsensusParameters cpv),
      -- | Exchange rates.
      pcpExchangeRates :: !ExchangeRates,
      -- | Cooldown parameters.
      pcpCooldownParameters :: !(CooldownParameters cpv),
      -- | Time parameters.
      pcpTimeParameters :: !(OParam 'PTTimeParameters cpv TimeParameters),
      -- | LimitAccountCreation: the maximum number of accounts that may be created in one block.
      pcpAccountCreationLimit :: !CredentialsPerBlockLimit,
      -- | Reward parameters.
      pcpRewardParameters :: !(RewardParameters cpv),
      -- | Foundation account index.
      pcpFoundationAccount :: !AccountIndex,
      -- | Parameters for baker pools.
      pcpPoolParameters :: !(PoolParameters cpv),
      -- | Finalization committee parameters.
      pcpFinalizationCommitteeParameters :: !(OParam 'PTFinalizationCommitteeParameters cpv FinalizationCommitteeParameters),
      -- | Validator score parameters.
      pcpValidatorScoreParameters :: !(OParam 'PTValidatorScoreParameters cpv ValidatorScoreParameters),
      -- | Rust-managed external chain parameters, present for P11-and-onwards authorization versions.
      pcpExternalChainParameters :: !(Conditionally (SupportsTokenParameters auv) ECP.ForeignExternalChainParametersPtr)
    }

-- | Protocol-indexed persistent node-owned chain parameters.
type PersistentChainParameters pv = PersistentChainParameters' (ChainParametersVersionFor pv) (AuthorizationsVersionFor pv)

-- | Convert a public/wire chain-parameter view and external pointer into the
-- persistent node representation.
fromChainParameters ::
    ChainParameters' cpv ->
    Conditionally (SupportsTokenParameters auv) ECP.ForeignExternalChainParametersPtr ->
    PersistentChainParameters' cpv auv
fromChainParameters ChainParameters{..} pcpExternalChainParameters =
    PersistentChainParameters
        { pcpConsensusParameters = _cpConsensusParameters,
          pcpExchangeRates = _cpExchangeRates,
          pcpCooldownParameters = _cpCooldownParameters,
          pcpTimeParameters = _cpTimeParameters,
          pcpAccountCreationLimit = _cpAccountCreationLimit,
          pcpRewardParameters = _cpRewardParameters,
          pcpFoundationAccount = _cpFoundationAccount,
          pcpPoolParameters = _cpPoolParameters,
          pcpFinalizationCommitteeParameters = _cpFinalizationCommitteeParameters,
          pcpValidatorScoreParameters = _cpValidatorScoreParameters,
          pcpExternalChainParameters = pcpExternalChainParameters
        }

-- | Construct persistent chain parameters from the public/wire view.
makePersistentChainParameters ::
    forall m cpv auv.
    (MonadBlobStore m, IsAuthorizationsVersion auv) =>
    ChainParameters' cpv ->
    m (PersistentChainParameters' cpv auv)
makePersistentChainParameters chainParameters = do
    externalChainParameters <- makeInitialExternalChainParameters @m @cpv @auv chainParameters
    return $ fromChainParameters chainParameters externalChainParameters

-- | Construct initial external chain parameters from the public chain-parameter view.
makeInitialExternalChainParameters ::
    forall m cpv auv.
    (MonadBlobStore m, IsAuthorizationsVersion auv) =>
    ChainParameters' cpv ->
    m (Conditionally (SupportsTokenParameters auv) ECP.ForeignExternalChainParametersPtr)
makeInitialExternalChainParameters ChainParameters{..} =
    case sSupportsTokenParameters (authorizationsVersion @auv) of
        SFalse -> return CFalse
        STrue ->
            CTrue <$> case _cpMaxLockDuration of
                SomeParam (Just duration) -> ECP.p11NewExternalChainParameters duration
                SomeParam Nothing -> error "P11 external chain parameters require max lock duration"
                NoParam -> error "P11 external chain parameters require max lock duration"

-- | Placeholder public-view value for the max-lock-duration field.
--
-- The authoritative P11 value is in the external chain-parameters component.
maxLockDurationPlaceholder :: SChainParametersVersion cpv -> OParam 'PTMaxLockDuration cpv (Maybe Duration)
maxLockDurationPlaceholder = \case
    SChainParametersV0 -> NoParam
    SChainParametersV1 -> NoParam
    SChainParametersV2 -> NoParam
    SChainParametersV3 -> SomeParam Nothing

-- | Convert persistent chain parameters to the public/wire view, using the
-- placeholder external fields.
persistentChainParametersToChainParameters ::
    forall cpv auv.
    (IsChainParametersVersion cpv) =>
    PersistentChainParameters' cpv auv ->
    ChainParameters' cpv
persistentChainParametersToChainParameters params =
    makeChainParametersView params (maxLockDurationPlaceholder (chainParametersVersion @cpv))

-- | Convert persistent chain parameters to the public/wire view, sourcing
-- externally-managed fields from the external chain-parameters component when present.
persistentChainParametersToChainParametersM ::
    forall m cpv auv.
    (MonadIO m, IsChainParametersVersion cpv) =>
    PersistentChainParameters' cpv auv ->
    m (ChainParameters' cpv)
persistentChainParametersToChainParametersM params@PersistentChainParameters{..} = do
    maxLockDuration <- case pcpExternalChainParameters of
        CFalse -> return $ maxLockDurationPlaceholder (chainParametersVersion @cpv)
        CTrue external -> case chainParametersVersion @cpv of
            SChainParametersV3 -> do
                duration <- liftIO $ ECP.getMaxLockDuration external
                return $ SomeParam (Just duration)
            _ -> return $ maxLockDurationPlaceholder (chainParametersVersion @cpv)
    return $ makeChainParametersView params maxLockDuration

-- | Construct the public/wire view from persistent fields and a supplied
-- max-lock-duration value.
makeChainParametersView ::
    PersistentChainParameters' cpv auv ->
    OParam 'PTMaxLockDuration cpv (Maybe Duration) ->
    ChainParameters' cpv
makeChainParametersView PersistentChainParameters{..} maxLockDuration =
    ChainParameters
        { _cpConsensusParameters = pcpConsensusParameters,
          _cpExchangeRates = pcpExchangeRates,
          _cpCooldownParameters = pcpCooldownParameters,
          _cpTimeParameters = pcpTimeParameters,
          _cpAccountCreationLimit = pcpAccountCreationLimit,
          _cpRewardParameters = pcpRewardParameters,
          _cpFoundationAccount = pcpFoundationAccount,
          _cpPoolParameters = pcpPoolParameters,
          _cpFinalizationCommitteeParameters = pcpFinalizationCommitteeParameters,
          _cpValidatorScoreParameters = pcpValidatorScoreParameters,
          _cpMaxLockDuration = maxLockDuration
        }

-- | Update the Haskell-managed chain parameters while preserving any
-- Rust-managed external chain-parameters pointer.
updateChainParameters ::
    ChainParameters' cpv ->
    PersistentChainParameters' cpv auv ->
    PersistentChainParameters' cpv auv
updateChainParameters newChainParameters PersistentChainParameters{..} =
    fromChainParameters newChainParameters pcpExternalChainParameters

-- | Serialize persistent chain parameters.
putPersistentChainParameters :: forall cpv auv. (IsChainParametersVersion cpv) => S.Putter (PersistentChainParameters' cpv auv)
putPersistentChainParameters PersistentChainParameters{..} = do
    withIsConsensusParametersVersionFor (chainParametersVersion @cpv) $ S.put pcpConsensusParameters
    S.put pcpExchangeRates
    putCooldownParameters pcpCooldownParameters
    S.put pcpTimeParameters
    S.put pcpAccountCreationLimit
    S.put pcpRewardParameters
    S.put pcpFoundationAccount
    putPoolParameters pcpPoolParameters
    S.put pcpFinalizationCommitteeParameters
    S.put pcpValidatorScoreParameters

-- | Deserialize persistent chain parameters, excluding the external component.
--
-- This is an internal helper function
getPersistentChainParametersFields :: forall cpv. (IsChainParametersVersion cpv) => S.Get (ChainParameters' cpv)
getPersistentChainParametersFields = do
    _cpConsensusParameters <- withIsConsensusParametersVersionFor (chainParametersVersion @cpv) S.get
    _cpExchangeRates <- S.get
    _cpCooldownParameters <- withIsCooldownParametersVersionFor (chainParametersVersion @cpv) S.get
    _cpTimeParameters <- S.get
    _cpAccountCreationLimit <- S.get
    _cpRewardParameters <- S.get
    _cpFoundationAccount <- S.get
    _cpPoolParameters <- withIsPoolParametersVersionFor (chainParametersVersion @cpv) S.get
    _cpFinalizationCommitteeParameters <- S.get
    _cpValidatorScoreParameters <- S.get
    let _cpMaxLockDuration = maxLockDurationPlaceholder (chainParametersVersion @cpv)
    return ChainParameters{..}

instance
    (MonadBlobStore m, IsChainParametersVersion cpv, IsAuthorizationsVersion auv) =>
    BlobStorable m (PersistentChainParameters' cpv auv)
    where
    storeUpdate params@PersistentChainParameters{..} = do
        (pExternal :: S.Put, external') <- case pcpExternalChainParameters of
            CFalse -> return (return (), CFalse)
            CTrue external -> do
                (putExternal, external') <- storeUpdate external
                return (putExternal, CTrue external')
        let newParams = params{pcpExternalChainParameters = external'}
        return
            ( do
                putPersistentChainParameters params
                pExternal,
              newParams
            )
    load = do
        chainParameters <- getPersistentChainParametersFields
        mExternal <- conditionallyA (sSupportsTokenParameters (sing @auv)) load
        return $ do
            externalChainParameters <- sequenceA mExternal
            return $ fromChainParameters chainParameters externalChainParameters

instance
    (MonadBlobStore m) =>
    Cacheable m (PersistentChainParameters' cpv auv)
    where
    cache params@PersistentChainParameters{..} = do
        external' <- traverse cache pcpExternalChainParameters
        return params{pcpExternalChainParameters = external'}

instance
    (MonadBlobStore m, IsChainParametersVersion cpv) =>
    MHashableTo m H.Hash (PersistentChainParameters' cpv auv)
    where
    getHashM params@PersistentChainParameters{..} = do
        hExternal <- traverse (getHashM @_ @ECP.ExternalChainParametersHash) pcpExternalChainParameters
        return $
            H.hash $
                S.runPut (putPersistentChainParameters params)
                    <> externalHashBytes hExternal
      where
        externalHashBytes :: Conditionally b ECP.ExternalChainParametersHash -> BS.ByteString
        externalHashBytes = \case
            CFalse -> mempty
            CTrue (ECP.ExternalChainParametersHash h) -> H.hashToByteString h
