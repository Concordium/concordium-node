{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{- |Part of the implementation of the GRPC2 interface This module constructs
    responses to queries that are handled by the Haskell part of the code.
-}
module Concordium.GRPC2 where

import Control.Concurrent
import Control.Monad
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString as BS
import Data.Int
import qualified Data.Serialize as S
import Data.Word
import Foreign

import Concordium.Types
import Concordium.ID.Types
import Concordium.Crypto.EncryptedTransfers
import Concordium.Types.Accounts

import qualified Concordium.External as Ext -- TODO: This is not an ideal configuration.
import Concordium.MultiVersion (
    Callbacks (..),
    CatchUpConfiguration (..),
    DiskStateConfig (..),
    MVR (..),
    MultiVersionConfiguration (..),
    MultiVersionRunner (..),
    makeMultiVersionRunner,
 )
import qualified Concordium.Queries as Q
import Concordium.Skov (
    UpdateResult (..), SkovQueryMonad (lastFinalizedBlock),
 )
import qualified Concordium.Types.Queries as Q

import Concordium.Types.Block
import qualified Data.ProtoLens as Proto
import qualified Data.ProtoLens.Combinators as Proto
import Lens.Micro.Platform
import qualified Data.ProtoLens.Field
import qualified Proto.Concordium.Types as Proto
import qualified Proto.Concordium.Types_Fields as ProtoFields
import Data.Coerce
import qualified Data.Sequence as Seq
import Data.Foldable
import Concordium.Types.Accounts.Releases
import Concordium.Types.Execution
import qualified Data.Map.Strict as Map
import Concordium.Common.Version
import Concordium.Common.Time
import Control.Monad.IO.Class
import Concordium.Crypto.SHA256 (Hash(Hash), DigestSize)
import qualified Data.FixedByteString as FBS

data ReceiverVec

data SenderChannel

type CopyToVecCallback = Ptr ReceiverVec -> Ptr Word8 -> Int64 -> IO ()
type ChannelSendCallback = Ptr SenderChannel -> Ptr Word8 -> Int64 -> IO Int32

-- |Boilerplate wrappers to invoke C callbacks.
foreign import ccall "dynamic" callChannelSendCallback :: FunPtr ChannelSendCallback -> ChannelSendCallback
foreign import ccall "dynamic" callCopyToVecCallback :: FunPtr CopyToVecCallback -> CopyToVecCallback

mkSerialize ::
    ( Proto.Message b
    , Data.ProtoLens.Field.HasField
        b
        "value"
        BS.ByteString
    , S.Serialize a
    ) =>
    a ->
    b
mkSerialize ek = Proto.defMessage & ProtoFields.value .~ S.encode ek

mkWord64 ::
    ( Proto.Message b
    , Data.ProtoLens.Field.HasField
        b
        "value"
        Word64
    , Coercible a Word64
    ) =>
    a ->
    b
mkWord64 a = Proto.defMessage & ProtoFields.value .~ coerce a

mkWord32 ::
    ( Proto.Message b
    , Data.ProtoLens.Field.HasField
        b
        "value"
        Word32
    , Coercible a Word32
    ) =>
    a ->
    b
mkWord32 a = Proto.defMessage & ProtoFields.value .~ coerce a

mkWord8 :: forall a b .
    ( Proto.Message b
    , Data.ProtoLens.Field.HasField
        b
        "value"
        Word32
    , Coercible a Word8
    ) =>
    a ->
    b
mkWord8 a = Proto.defMessage & ProtoFields.value .~ (fromIntegral (coerce a :: Word8) :: Word32)

mkCredentials :: Map.Map CredentialIndex (Versioned RawAccountCredential) -> Map.Map Word32 Proto.AccountCredential
mkCredentials = Map.fromAscList . map convert . Map.toAscList
  where convert (ci, Versioned version rac) = (fromIntegral ci, convertCredential rac)
        convertCredential :: RawAccountCredential -> Proto.AccountCredential
        convertCredential (InitialAC InitialCredentialDeploymentValues{..}) =
          Proto.make $ ProtoFields.initial .= Proto.make (do
            ProtoFields.keys .= mkKeys icdvAccount
            ProtoFields.credId .= mkSerialize icdvRegId
            ProtoFields.ipId .= mkWord32 icdvIpId
            ProtoFields.policy .= mkPolicy icdvPolicy
                                                         )
        convertCredential (NormalAC CredentialDeploymentValues{..} commitments) = Proto.make $ ProtoFields.normal .= Proto.make (do
            ProtoFields.keys .= mkKeys cdvPublicKeys
            ProtoFields.credId .= mkSerialize cdvCredId
            ProtoFields.ipId .= mkWord32 cdvIpId
            ProtoFields.policy .= mkPolicy cdvPolicy
            ProtoFields.arThreshold .= mkWord8 cdvThreshold)
        mkPolicy Policy{..} = Proto.make $ do
          ProtoFields.createdAt .= mkYearMonth pCreatedAt
          ProtoFields.validTo .= mkYearMonth pValidTo
          ProtoFields.attributes .= mkAttributes pItems

        mkYearMonth YearMonth{..} = Proto.make $ do
            ProtoFields.year .= fromIntegral ymYear
            ProtoFields.month .= fromIntegral ymMonth

        mkKeys :: CredentialPublicKeys -> Proto.CredentialPublicKeys
        mkKeys CredentialPublicKeys{..} = Proto.make $ do
          ProtoFields.threshold .= mkWord8 credThreshold
          ProtoFields.keys .= (Map.fromAscList . map convertKey . Map.toAscList $ credKeys)
            where convertKey (ki, key) = (fromIntegral ki, Proto.make $ ProtoFields.ed25519Key .= S.encode key)

        mkAttributes = Map.fromAscList .
                       map (\(AttributeTag tag, value) -> (fromIntegral tag, S.runPut (S.putShortByteString (coerce value)))) .
                       Map.toAscList

mkAccountInfo :: AccountInfo -> Proto.AccountInfo
mkAccountInfo ai = Proto.make $ do
    ProtoFields.nonce .= mkWord64 (aiAccountNonce ai)
    ProtoFields.amount .= mkWord64 (aiAccountAmount ai)
    ProtoFields.schedule .= mkSchedule
    ProtoFields.creds .= mkCredentials (aiAccountCredentials ai)
    ProtoFields.threshold .= mkWord8 (aiAccountThreshold ai)
    ProtoFields.encryptedBalance .= mkEncryptedBalance'
    ProtoFields.encryptionKey .= mkSerialize (aiAccountEncryptionKey ai)
    ProtoFields.index .= mkWord64 (aiAccountIndex ai)
    ProtoFields.address .= mkSerialize (aiAccountAddress ai)
    maybeAddStakingInfo
    where encBal = aiAccountEncryptedAmount ai
          mkEncryptedBalance = do
              ProtoFields.selfAmount .= mkSerialize (_selfAmount encBal)
              ProtoFields.startIndex .= coerce (_startIndex encBal)
              ProtoFields.incomingAmounts .= (mkSerialize <$> toList (_incomingEncryptedAmounts encBal))
          mkEncryptedBalance' :: Proto.EncryptedBalance
          mkEncryptedBalance' =
            case _aggregatedAmount encBal of
              Nothing -> Proto.make mkEncryptedBalance
              Just (aggAmount, numAgg) -> Proto.make $ do
                  mkEncryptedBalance
                  ProtoFields.aggregatedAmount .= mkSerialize aggAmount
                  ProtoFields.numAggregated .= numAgg
          mkSchedule :: Proto.ReleaseSchedule
          mkSchedule = Proto.make $ do
              ProtoFields.total .= mkWord64 (releaseTotal (aiAccountReleaseSchedule ai))
              ProtoFields.schedules .= (mkRelease <$> releaseSchedule (aiAccountReleaseSchedule ai))
              where mkRelease r = Proto.make $ do
                        ProtoFields.timestamp .= coerce (releaseTimestamp r)
                        ProtoFields.amount .= mkWord64 (releaseAmount r)
                        ProtoFields.transactions .= (mkSerialize <$> releaseTransactions r)
          maybeAddStakingInfo = case aiStakingInfo ai of
            AccountStakingNone -> return ()
            AccountStakingBaker{asiBakerInfo=BakerInfo{..},..} -> do
              ProtoFields.stake .= Proto.make (do
                ProtoFields.baker .= Proto.make (do
                       ProtoFields.stakedAmount .= mkWord64 asiStakedAmount
                       ProtoFields.restakeEarnings .= asiStakeEarnings
                       ProtoFields.bakerInfo .= Proto.make (do
                                                     ProtoFields.bakerId .= mkWord64 _bakerIdentity
                                                     ProtoFields.electionKey .= mkSerialize _bakerElectionVerifyKey
                                                     ProtoFields.signatureKey .= mkSerialize _bakerSignatureVerifyKey
                                                     ProtoFields.aggregationKey .= mkSerialize _bakerAggregationVerifyKey
                                                     )
                       -- TODO: pending_change, pool_info
                      ))
            AccountStakingDelegated{..} ->
              ProtoFields.stake .= Proto.make (do
                ProtoFields.delegate .= Proto.make (do
                  ProtoFields.stakedAmount .= mkWord64 asiStakedAmount
                  ProtoFields.restakeEarnings .= asiStakeEarnings
                  ProtoFields.target .= Proto.make (do
                                                 case asiDelegationTarget of
                                                   DelegatePassive -> ProtoFields.passive .= Proto.defMessage
                                                   DelegateToBaker bi -> ProtoFields.baker .= mkWord64 bi
                                        )))
                                -- TODO: StakePendingChange

-- |NB: Assumes the data is at least 32 bytes
decodeBlockHashInput :: Word8 -> Ptr Word8 -> IO Q.BlockHashInput
decodeBlockHashInput 0 _ = return Q.BHIBest
decodeBlockHashInput 1 _ = return Q.BHILastFinal
decodeBlockHashInput _ hsh = Q.BHIGiven . coerce <$> FBS.create @DigestSize (\p -> copyBytes p hsh 32)

decodeAccountIdentifierInput :: Word8 -> Ptr Word8 -> IO AccountIdentifier
decodeAccountIdentifierInput 0 dta = AccAddress . coerce <$> FBS.create @AccountAddressSize (\p -> copyBytes p dta 32)
decodeAccountIdentifierInput 1 dta = do
  bs <- BS.unsafePackCStringLen (castPtr dta, 48)
  case S.decode bs of
    Left err -> error $ "Precondition violation in FFI call: " ++ err
    Right cid -> return (CredRegID cid)
decodeAccountIdentifierInput 2 dta = AccIndex . AccountIndex <$> peek (castPtr dta)
decodeAccountIdentifierInput n _ = error $ "Unknown account identifier tag: " ++ show n

data QueryResult =
  QRSuccess
  | QRNotFound

queryResultCode :: QueryResult -> Int64
queryResultCode QRSuccess = 0
queryResultCode QRNotFound = 1

getAccountInfoV2 :: StablePtr Ext.ConsensusRunner
                 -> Word8 -- ^Identifier type, 0 for account address, 1 for credential, 2 for account index
                 -> Ptr Word8 -- ^Serialized identifier. Length determined by the type.
                 -> Word8 -- ^Block type.
                 -> Ptr Word8 -- ^Block hash.
                 -> Ptr ReceiverVec
                 -> FunPtr CopyToVecCallback -- ^Callback to output data.
                 -> IO Int64
getAccountInfoV2 cptr accIdType accIdBytesPtr blockType blockHashPtr outVec copierCbk = do
    Ext.ConsensusRunner mvr <- deRefStablePtr cptr
    let copier = callCopyToVecCallback copierCbk
    bhi <- decodeBlockHashInput blockType blockHashPtr
    ai <- decodeAccountIdentifierInput accIdType accIdBytesPtr
    mai <- runMVR (Q.getAccountInfo bhi ai) mvr
    case mai of
      Nothing -> return (queryResultCode QRNotFound)
      Just ainfo -> do
        let encoded = Proto.encodeMessage (mkAccountInfo ainfo)
        BS.unsafeUseAsCStringLen encoded (\(ptr, len) -> copier outVec (castPtr ptr) (fromIntegral len))
        return (queryResultCode QRSuccess)

-- A POC that "streams finalized blocks". Each 0.1s it sends the current last finalized block until the channel is closed.
streamFinalized :: StablePtr Ext.ConsensusRunner -> Ptr SenderChannel -> FunPtr (Ptr SenderChannel -> Ptr Word8 -> Int64 -> IO Int32) -> IO Int64
streamFinalized cptr channel cbk = do
    Ext.ConsensusRunner mvr <- deRefStablePtr cptr
    let sender = callChannelSendCallback cbk
    tid <- forkIO $ do
        let go = do
                (cs, bi) <- flip runMVR mvr $ do
                  cs <- Q.getConsensusStatus
                  bi <- Q.getBlockInfo (Q.csLastFinalizedBlock cs)
                  return (cs, bi)
                let msg :: Proto.FinalizedBlockInfo
                    msg =
                        Proto.defMessage
                            & (ProtoFields.hash .~ mkSerialize (Q.csLastFinalizedBlock cs))
                                . (ProtoFields.height .~ mkWord64 (Q.csLastFinalizedBlockHeight cs))
                                . (ProtoFields.maybe'bakerId .~ (mkWord64 <$> (Q.biBlockBaker =<< bi)))
                    encoded = Proto.encodeMessage msg
                r <- BS.unsafeUseAsCStringLen encoded (\(ptr, len) -> sender channel (castPtr ptr) (fromIntegral len))
                when (r == 0 || r == -1) $ do
                    threadDelay 100000
                    go
        go

    return (Ext.toReceiveResult ResultSuccess)

-- * Foreign exports

foreign export ccall getAccountInfoV2 :: StablePtr Ext.ConsensusRunner
                 -> Word8 -- ^Identifier type, 0 for account address, 1 for credential, 2 for account index
                 -> Ptr Word8 -- ^Serialized identifier. Length determined by the type.
                 -> Word8 -- ^Block type.
                 -> Ptr Word8 -- ^Block hash.
                 -> Ptr ReceiverVec -> FunPtr (Ptr ReceiverVec -> Ptr Word8 -> Int64 -> IO ()) -> IO Int64


foreign export ccall streamFinalized :: StablePtr Ext.ConsensusRunner -> Ptr SenderChannel -> FunPtr ChannelSendCallback -> IO Int64
