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
-- persistent storage model has its own record fields and a
-- Rust-managed external chain-parameters pointer.
module Concordium.GlobalState.Persistent.BlockState.Parameters (
    PersistentChainParameters (..),
    makePersistentChainParameters,
    persistentChainParametersToChainParametersM,
    updateChainParameters,
    executeExternalChainParameterUpdate,
) where

import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import qualified Data.Serialize as S

import qualified Concordium.Crypto.SHA256 as H
import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.GlobalState.Persistent.BlockState.ExternalChainParameters as ECP
import Concordium.Types
import Concordium.Types.Conditionally
import Concordium.Types.HashableTo
import Concordium.Types.Parameters
import qualified Concordium.Types.Updates as Updates

-- | Persistent node-owned chain parameters.
data PersistentChainParameters (pv :: ProtocolVersion) = PersistentChainParameters
    { -- | Consensus parameters.
      pcpConsensusParameters :: !(ConsensusParameters (ChainParametersVersionFor pv)),
      -- | Exchange rates.
      pcpExchangeRates :: !ExchangeRates,
      -- | Cooldown parameters.
      pcpCooldownParameters :: !(CooldownParameters (ChainParametersVersionFor pv)),
      -- | Time parameters.
      pcpTimeParameters :: !(OParam 'PTTimeParameters (ChainParametersVersionFor pv) TimeParameters),
      -- | LimitAccountCreation: the maximum number of accounts that may be created in one block.
      pcpAccountCreationLimit :: !CredentialsPerBlockLimit,
      -- | Reward parameters.
      pcpRewardParameters :: !(RewardParameters (ChainParametersVersionFor pv)),
      -- | Foundation account index.
      pcpFoundationAccount :: !AccountIndex,
      -- | Parameters for baker pools.
      pcpPoolParameters :: !(PoolParameters (ChainParametersVersionFor pv)),
      -- | Finalization committee parameters.
      pcpFinalizationCommitteeParameters :: !(OParam 'PTFinalizationCommitteeParameters (ChainParametersVersionFor pv) FinalizationCommitteeParameters),
      -- | Validator score parameters.
      pcpValidatorScoreParameters :: !(OParam 'PTValidatorScoreParameters (ChainParametersVersionFor pv) ValidatorScoreParameters),
      -- | Rust-managed external chain parameters, present when supported by the protocol.
      pcpExternalChainParameters :: !(Conditionally (SupportsRustManagedECP pv) (ECP.ForeignExternalChainParametersPtr pv))
    }

-- | Convert a public/wire chain-parameter view and external pointer into the
-- persistent node representation.
fromChainParameters ::
    ChainParameters pv ->
    Conditionally (SupportsRustManagedECP pv) (ECP.ForeignExternalChainParametersPtr pv) ->
    PersistentChainParameters pv
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
    forall m pv.
    (MonadBlobStore m, IsProtocolVersion pv) =>
    ChainParameters pv ->
    m (PersistentChainParameters pv)
makePersistentChainParameters chainParameters = do
    externalChainParameters <- makeInitialExternalChainParameters @m @pv chainParameters
    return $ fromChainParameters chainParameters externalChainParameters

-- | Construct initial external chain parameters from the public chain-parameter view.
makeInitialExternalChainParameters ::
    forall m pv.
    (MonadBlobStore m, IsProtocolVersion pv) =>
    ChainParameters pv ->
    m (Conditionally (SupportsRustManagedECP pv) (ECP.ForeignExternalChainParametersPtr pv))
makeInitialExternalChainParameters chainParameters = case protocolVersion @pv of
    SP1 -> return CFalse
    SP2 -> return CFalse
    SP3 -> return CFalse
    SP4 -> return CFalse
    SP5 -> return CFalse
    SP6 -> return CFalse
    SP7 -> return CFalse
    SP8 -> return CFalse
    SP9 -> return CFalse
    SP10 -> return CFalse
    SP11 -> CTrue <$> ECP.p11NewExternalChainParameters (unOParam (_cpMaxLockDuration chainParameters))

-- | Convert persistent chain parameters to the public/wire view, sourcing
-- externally-managed fields from the external chain-parameters component when present.
persistentChainParametersToChainParametersM ::
    forall m pv.
    (MonadIO m, IsProtocolVersion pv) =>
    PersistentChainParameters pv ->
    m (ChainParameters pv)
persistentChainParametersToChainParametersM params@PersistentChainParameters{..} = do
    maxLockDuration <- case (protocolVersion @pv, pcpExternalChainParameters) of
        (SP1, CFalse) -> return NoParam
        (SP2, CFalse) -> return NoParam
        (SP3, CFalse) -> return NoParam
        (SP4, CFalse) -> return NoParam
        (SP5, CFalse) -> return NoParam
        (SP6, CFalse) -> return NoParam
        (SP7, CFalse) -> return NoParam
        (SP8, CFalse) -> return NoParam
        (SP9, CFalse) -> return NoParam
        (SP10, CFalse) -> return NoParam
        (SP11, CTrue external) -> do
            duration <- liftIO $ ECP.getMaxLockDuration external
            return $ SomeParam duration
    return $ makeChainParametersView params maxLockDuration

-- | Construct the public/wire view from persistent fields and a supplied
-- max-lock-duration value.
makeChainParametersView ::
    PersistentChainParameters pv ->
    OParam 'PTMaxLockDuration (ChainParametersVersionFor pv) Duration ->
    ChainParameters pv
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
    ChainParameters pv ->
    PersistentChainParameters pv ->
    PersistentChainParameters pv
updateChainParameters newChainParameters PersistentChainParameters{..} =
    fromChainParameters newChainParameters pcpExternalChainParameters

-- | Execute a chain update against the Rust-managed external chain parameters.
--
-- The current parameters are left unchanged. An unexpected payload, absence of
-- external chain parameters, or a Rust panic terminates execution with an error.
--
-- @
-- updated <- executeExternalChainParameterUpdate (Updates.MaxLockDurationUpdatePayload duration) current
-- @
executeExternalChainParameterUpdate ::
    (MonadIO m) =>
    Updates.UpdatePayload ->
    PersistentChainParameters pv ->
    m (PersistentChainParameters pv)
executeExternalChainParameterUpdate payload params@PersistentChainParameters{pcpExternalChainParameters = CTrue external} = do
    newExternal <- liftIO $ ECP.executeChainUpdate external payload
    return params{pcpExternalChainParameters = CTrue newExternal}
executeExternalChainParameterUpdate _ PersistentChainParameters{pcpExternalChainParameters = CFalse} =
    error "External chain parameter updates require external chain parameters"

-- | Serialize persistent chain parameters.
putPersistentChainParameters :: forall pv. (IsProtocolVersion pv) => S.Putter (PersistentChainParameters pv)
putPersistentChainParameters PersistentChainParameters{..} = do
    withIsConsensusParametersVersionFor (chainParametersVersion @(ChainParametersVersionFor pv)) $ S.put pcpConsensusParameters
    S.put pcpExchangeRates
    putCooldownParameters pcpCooldownParameters
    S.put pcpTimeParameters
    S.put pcpAccountCreationLimit
    S.put pcpRewardParameters
    S.put pcpFoundationAccount
    putPoolParameters pcpPoolParameters
    S.put pcpFinalizationCommitteeParameters
    S.put pcpValidatorScoreParameters

-- | Read the Haskell-managed fields and return a constructor for the full persistent value.
-- The caller must supply the separately loaded external component.
getPersistentChainParametersFields ::
    forall pv.
    (IsProtocolVersion pv) =>
    S.Get (Conditionally (SupportsRustManagedECP pv) (ECP.ForeignExternalChainParametersPtr pv) -> PersistentChainParameters pv)
getPersistentChainParametersFields = do
    pcpConsensusParameters <- withIsConsensusParametersVersionFor (chainParametersVersion @(ChainParametersVersionFor pv)) S.get
    pcpExchangeRates <- S.get
    pcpCooldownParameters <- withIsCooldownParametersVersionFor (chainParametersVersion @(ChainParametersVersionFor pv)) S.get
    pcpTimeParameters <- S.get
    pcpAccountCreationLimit <- S.get
    pcpRewardParameters <- S.get
    pcpFoundationAccount <- S.get
    pcpPoolParameters <- withIsPoolParametersVersionFor (chainParametersVersion @(ChainParametersVersionFor pv)) S.get
    pcpFinalizationCommitteeParameters <- S.get
    pcpValidatorScoreParameters <- S.get
    return $ \pcpExternalChainParameters -> PersistentChainParameters{..}

instance
    (MonadBlobStore m, IsProtocolVersion pv) =>
    BlobStorable m (PersistentChainParameters pv)
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
        makeParameters <- getPersistentChainParametersFields @pv
        mExternal <- conditionallyA (sSupportsRustManagedECP (protocolVersion @pv)) load
        return $ do
            externalChainParameters <- sequenceA mExternal
            return $ makeParameters externalChainParameters

instance
    (MonadBlobStore m) =>
    Cacheable m (PersistentChainParameters pv)
    where
    cache params@PersistentChainParameters{..} = do
        external' <- traverse cache pcpExternalChainParameters
        return params{pcpExternalChainParameters = external'}

instance
    (MonadBlobStore m, IsProtocolVersion pv) =>
    MHashableTo m H.Hash (PersistentChainParameters pv)
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
