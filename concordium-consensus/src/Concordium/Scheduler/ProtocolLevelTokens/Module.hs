{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Concordium.Scheduler.ProtocolLevelTokens.Module where

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS.Builder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Sequence as Seq
import Data.Serialize
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word

import Concordium.Cost
import Concordium.Types
import Concordium.Types.ProtocolLevelTokens.CBOR
import Concordium.Types.Tokens

import Concordium.Scheduler.ProtocolLevelTokens.Kernel

-- | The context for a token-holder or token-governance transaction.
data TransactionContext' account = TransactionContext
    { -- | The sender account.
      tcSender :: !account,
      -- | The sender account address. This is the account alias that is used by the transaction
      --  itself.
      tcSenderAddress :: !AccountAddress
    }

type TransactionContext m = TransactionContext' (PLTAccount m)

-- | Represents the reasons why 'initializeToken' can fail.
data InitializeTokenError
    = ITEDeserializationFailure !String
    | ITEInvalidMintAmount !String
    | ITEGovernanceAccountDoesNotExist !AccountAddress
    deriving (Eq)

instance Show InitializeTokenError where
    show (ITEDeserializationFailure reason) =
        "Token initialization parameters could not be deserialized: " ++ reason
    show (ITEInvalidMintAmount reason) =
        "The initial mint amount was not valid: " ++ reason
    show (ITEGovernanceAccountDoesNotExist account) =
        "The given governance account does not exist: " ++ show account

-- | Try to convert a 'TokenAmount' to a 'TokenRawAmount'. The latter is represented in the
--  smallest subdivision allowed for the current token.
--  This can fail if:
--
--   - The token amount specifies more decimals than the token allows in its representation.
--   - The amount would be outside of the representable range as a 'TokenRawAmount'.
toTokenRawAmount ::
    -- | The number of decimals in the token representation.
    Word8 ->
    TokenAmount ->
    Either String TokenRawAmount
toTokenRawAmount actualDecimals TokenAmount{..}
    | actualDecimals == taDecimals = Right taValue
    | otherwise = Left "Token amount precision mismatch"

-- | Convert a 'TokenRawAmount' to a 'TokenAmount' given the number of decimals in the
--  representation.
toTokenAmount ::
    -- | Number of decimals
    Word8 ->
    -- | Amount to convert
    TokenRawAmount ->
    -- | Converted amount
    TokenAmount
toTokenAmount decimals rawAmount =
    TokenAmount
        { taValue = rawAmount,
          taDecimals = decimals
        }

-- | Initialize a PLT by recording the relevant configuration parameters in the state and
--  (if necessary) minting the initial supply to the token governance account.
initializeToken ::
    (PLTKernelPrivilegedUpdate m, PLTKernelFail InitializeTokenError m, Monad m) =>
    TokenParameter ->
    m ()
initializeToken tokenParam = do
    case tokenInitializationParametersFromBytes tokenParamLBS of
        Left failureReason -> pltError $ ITEDeserializationFailure failureReason
        Right TokenInitializationParameters{..} -> do
            setTokenState "name" (Just $ Text.encodeUtf8 tipName)
            setTokenState "metadata" (Just $ tokenMetadataUrlToBytes tipMetadata)
            when tipAllowList $ setTokenState "allowList" (Just "")
            when tipDenyList $ setTokenState "denyList" (Just "")
            when tipMintable $ setTokenState "mintable" (Just "")
            when tipBurnable $ setTokenState "burnable" (Just "")
            mbGovAccount <- getAccount tipGovernanceAccount
            case mbGovAccount of
                Nothing ->
                    pltError (ITEGovernanceAccountDoesNotExist tipGovernanceAccount)
                Just govAccount -> do
                    setTokenState "governanceAccount" (Just $ encode tipGovernanceAccount)
                    forM_ tipInitialSupply $ \initSupply -> do
                        decimals <- getDecimals
                        case toTokenRawAmount decimals initSupply of
                            Left reason -> pltError (ITEInvalidMintAmount reason)
                            Right amt -> do
                                mintOK <- mint govAccount amt
                                unless mintOK $ pltError (ITEInvalidMintAmount "Kernel failed to mint")
  where
    tokenParamLBS =
        BS.Builder.toLazyByteString $ BS.Builder.shortByteString $ parameterBytes tokenParam

-- | A pre-processed token operation. This has all amounts converted to
--  'TokenRawAmount's and unwraps the metadata associated with target accounts.
data PreprocessedTokenOperation
    = PTOTransfer
        { -- | The raw amount to transfer.
          pthoAmount :: !TokenRawAmount,
          -- | The recipient account address.
          pthoRecipient :: !TokenHolder,
          -- | The (optional) memo.
          pthoMemo :: !(Maybe Memo),
          -- | The original, unprocessed, 'TokenTransferBody'.
          pthoUnprocessed :: !TokenTransferBody
        }
    | PTOTokenMint
        { ptgoAmount :: !TokenRawAmount,
          ptgoUnprocessedAmount :: !TokenAmount
        }
    | PTOTokenBurn
        { ptgoAmount :: !TokenRawAmount,
          ptgoUnprocessedAmount :: !TokenAmount
        }
    | PTOTokenAddAllowList
        {ptgoTarget :: !TokenHolder}
    | PTOTokenRemoveAllowList
        {ptgoTarget :: !TokenHolder}
    | PTOTokenAddDenyList
        {ptgoTarget :: !TokenHolder}
    | PTOTokenRemoveDenyList
        {ptgoTarget :: !TokenHolder}
    deriving (Eq, Show)

preprocessTokenTransaction ::
    (PLTKernelFail EncodedTokenRejectReason m, Monad m) =>
    Word8 ->
    TokenTransaction ->
    m (Seq.Seq PreprocessedTokenOperation)
preprocessTokenTransaction decimals = mapM preproc . tokenOperations
  where
    preproc (TokenTransfer ttb@(TokenTransferBody{..})) =
        case toTokenRawAmount decimals ttAmount of
            Left err ->
                pltError . encodeTokenRejectReason . DeserializationFailure . Just . Text.pack $
                    "Token amount outside representable range: " ++ err
            Right pthoAmount -> return PTOTransfer{..}
              where
                pthoRecipient = ttRecipient
                pthoMemo = taggableMemoInner <$> ttMemo
                pthoUnprocessed = ttb
    preproc (TokenMint amount) =
        case toTokenRawAmount decimals amount of
            Left err ->
                pltError . encodeTokenRejectReason . DeserializationFailure . Just . Text.pack $
                    "Token mint amount outside representable range: " ++ err
            Right ptgoAmount -> return PTOTokenMint{..}
      where
        ptgoUnprocessedAmount = amount
    preproc (TokenBurn amount) =
        case toTokenRawAmount decimals amount of
            Left err ->
                pltError . encodeTokenRejectReason . DeserializationFailure . Just . Text.pack $
                    "Token burn amount outside representable range: " ++ err
            Right ptgoAmount -> return PTOTokenBurn{..}
      where
        ptgoUnprocessedAmount = amount
    preproc (TokenAddAllowList receiver) =
        return PTOTokenAddAllowList{ptgoTarget = receiver}
    preproc (TokenRemoveAllowList receiver) =
        return PTOTokenRemoveAllowList{ptgoTarget = receiver}
    preproc (TokenAddDenyList receiver) =
        return PTOTokenAddDenyList{ptgoTarget = receiver}
    preproc (TokenRemoveDenyList receiver) =
        return PTOTokenRemoveDenyList{ptgoTarget = receiver}

-- | Encode and log a 'TokenEvent'.
logEncodeTokenEvent :: (PLTKernelUpdate m) => TokenEvent -> m ()
logEncodeTokenEvent te = logTokenEvent eventType details
  where
    EncodedTokenEvent{eteType = eventType, eteDetails = details} = encodeTokenEvent te

-- | Execute a token-governance transaction. The process is as follows:
--
--   - Decode the transaction CBOR parameter.
--   - Check that amounts are within the representable range.
--   - For each transfer operation:
--
--       - Check that the recipient is valid.
--       - Transfer the amount from the sender to the recipient, if the sender's balance is
--         sufficient.
executeTokenTransaction ::
    (PLTKernelPrivilegedUpdate m, PLTKernelChargeEnergy m, PLTKernelFail EncodedTokenRejectReason m, Monad m) =>
    TransactionContext m ->
    TokenParameter ->
    m ()
executeTokenTransaction TransactionContext{..} tokenParam = do
    case tokenTransactionFromBytes tokenParamLBS of
        Left failureReason -> failTH $ DeserializationFailure $ Just $ Text.pack failureReason
        Right parsedTransaction -> do
            decimals <- getDecimals
            operations <- preprocessTokenTransaction decimals parsedTransaction
            let handleOperation !opIndex op = do
                    case op of
                        PTOTransfer{..} -> do
                            recipientAccount <- requireAccount opIndex pthoRecipient
                            -- If the allow list is enabled, check that the sender and recipient are
                            -- both on the allow list.
                            enforceAllowList <- isJust <$> getTokenState "allowList"
                            when enforceAllowList $ do
                                senderAllowed <- isJust <$> getAccountState tcSender "allowList"
                                unless senderAllowed $ do
                                    failTH
                                        OperationNotPermitted
                                            { trrOperationIndex = opIndex,
                                              trrAddressNotPermitted =
                                                Just (accountTokenHolder tcSenderAddress),
                                              trrReason = Just "sender not in allow list"
                                            }
                                recipientAllowed <- isJust <$> getAccountState recipientAccount "allowList"
                                unless recipientAllowed $ do
                                    failTH
                                        OperationNotPermitted
                                            { trrOperationIndex = opIndex,
                                              trrAddressNotPermitted = Just pthoRecipient,
                                              trrReason = Just "recipient not in allow list"
                                            }
                            enforceDenyList <- isJust <$> getTokenState "denyList"
                            -- If the deny list is enabled, check that neither the sender nor the
                            -- recipient are on the deny list.
                            when enforceDenyList $ do
                                senderDenied <- isJust <$> getAccountState tcSender "denyList"
                                when senderDenied $ do
                                    failTH
                                        OperationNotPermitted
                                            { trrOperationIndex = opIndex,
                                              trrAddressNotPermitted =
                                                Just (accountTokenHolder tcSenderAddress),
                                              trrReason = Just "sender in deny list"
                                            }
                                recipientDenied <- isJust <$> getAccountState recipientAccount "denyList"
                                when recipientDenied $ do
                                    failTH
                                        OperationNotPermitted
                                            { trrOperationIndex = opIndex,
                                              trrAddressNotPermitted = Just pthoRecipient,
                                              trrReason = Just "recipient in deny list"
                                            }
                            pltTickEnergy tokenTransferCost
                            success <- transfer tcSender recipientAccount pthoAmount pthoMemo
                            unless success $ do
                                availableBalance <- getAccountBalance tcSender
                                failTH
                                    TokenBalanceInsufficient
                                        { trrOperationIndex = opIndex,
                                          trrAvailableBalance = toTokenAmount decimals availableBalance,
                                          trrRequiredBalance = ttAmount pthoUnprocessed
                                        }
                        tokenGovernanceOp -> do
                            mbGovAccount <- getTokenState "governanceAccount"
                            case mbGovAccount of
                                Nothing -> error "Invariant violation: Token state is missing the issuer account address."
                                Just govAccount -> do
                                    unless (govAccount == encode tcSenderAddress) $
                                        failTH
                                            OperationNotPermitted
                                                { trrOperationIndex = opIndex,
                                                  trrAddressNotPermitted =
                                                    Just (accountTokenHolder tcSenderAddress),
                                                  trrReason = Just "sender is not token issuer"
                                                }
                                    case tokenGovernanceOp of
                                        PTOTokenMint{..} -> do
                                            requireFeature opIndex "mint" "mintable"
                                            pltTickEnergy tokenMintCost
                                            mintOK <- mint tcSender ptgoAmount
                                            unless mintOK $ do
                                                availableSupply <- getCirculatingSupply
                                                failTH
                                                    MintWouldOverflow
                                                        { trrOperationIndex = opIndex,
                                                          trrRequestedAmount = ptgoUnprocessedAmount,
                                                          trrCurrentSupply = toTokenAmount decimals availableSupply,
                                                          trrMaxRepresentableAmount =
                                                            toTokenAmount decimals (maxBound :: TokenRawAmount)
                                                        }
                                        PTOTokenBurn{..} -> do
                                            requireFeature opIndex "burn" "burnable"
                                            pltTickEnergy tokenBurnCost
                                            burnOK <- burn tcSender ptgoAmount
                                            unless burnOK $ do
                                                availableBalance <- getAccountBalance tcSender
                                                failTH
                                                    TokenBalanceInsufficient
                                                        { trrOperationIndex = opIndex,
                                                          trrAvailableBalance = toTokenAmount decimals availableBalance,
                                                          trrRequiredBalance = ptgoUnprocessedAmount
                                                        }
                                        PTOTokenAddAllowList{..} -> do
                                            requireFeature opIndex "addAllowList" "allowList"
                                            account <- requireAccount opIndex ptgoTarget
                                            pltTickEnergy tokenListOperationCost
                                            setAccountState account "allowList" (Just "")
                                            logEncodeTokenEvent (AddAllowListEvent ptgoTarget)
                                        PTOTokenRemoveAllowList{..} -> do
                                            requireFeature opIndex "removeAllowList" "allowList"
                                            account <- requireAccount opIndex ptgoTarget
                                            pltTickEnergy tokenListOperationCost
                                            setAccountState account "allowList" Nothing
                                            logEncodeTokenEvent (RemoveAllowListEvent ptgoTarget)
                                        PTOTokenAddDenyList{..} -> do
                                            requireFeature opIndex "addDenyList" "denyList"
                                            account <- requireAccount opIndex ptgoTarget
                                            pltTickEnergy tokenListOperationCost
                                            setAccountState account "denyList" (Just "")
                                            logEncodeTokenEvent (AddDenyListEvent ptgoTarget)
                                        PTOTokenRemoveDenyList{..} -> do
                                            requireFeature opIndex "removeDenyList" "denyList"
                                            account <- requireAccount opIndex ptgoTarget
                                            pltTickEnergy tokenListOperationCost
                                            setAccountState account "denyList" Nothing
                                            logEncodeTokenEvent (RemoveDenyListEvent ptgoTarget)
                    return (opIndex + 1)
            foldM_ handleOperation 0 operations
  where
    tokenParamLBS =
        BS.Builder.toLazyByteString $ BS.Builder.shortByteString $ parameterBytes tokenParam
    failTH = pltError . encodeTokenRejectReason

-- | Check that a particular feature is enabled for the token, and otherwise fail with
--  'UnsupportedOperation'.
requireFeature ::
    (PLTKernelFail EncodedTokenRejectReason m, PLTKernelQuery m, Monad m) =>
    -- | The operation index.
    Word64 ->
    -- | The operation name.
    Text.Text ->
    -- | The feature to check.
    TokenStateKey ->
    m ()
requireFeature trrOperationIndex trrOperationType feature = do
    featureState <- getTokenState feature
    when (isNothing featureState) $
        pltError . encodeTokenRejectReason $
            UnsupportedOperation{trrReason = Just "feature not enabled", ..}

-- | Check that an account is valid and return the corresponding 'PLTAccount'.
--  This will fail with 'AddressNotFound' if the account is not valid.
requireAccount ::
    (PLTKernelFail EncodedTokenRejectReason m, PLTKernelQuery m, Monad m) =>
    -- | The operation index.
    Word64 ->
    -- | The account to check.
    TokenHolder ->
    m (PLTAccount m)
requireAccount trrOperationIndex holder@HolderAccount{..} = do
    getAccount holderAccountAddress >>= \case
        Nothing ->
            pltError . encodeTokenRejectReason $
                AddressNotFound{trrAddress = holder, ..}
        Just acc -> return acc

-- | An error that may be when querying the token state.
newtype QueryTokenError
    = -- | A state invariant was violated.
      QTEInvariantViolation String
    deriving (Eq)

instance Show QueryTokenError where
    show (QTEInvariantViolation reason) = "Token state invariant violation: " ++ reason

-- | Get the CBOR-encoded representation of the token module state.
queryTokenModuleState :: (PLTKernelQuery m, PLTKernelFail QueryTokenError m, Monad m) => m BS.ByteString
queryTokenModuleState = do
    tmsName <-
        getTokenState "name" >>= \case
            Nothing -> pltError $ QTEInvariantViolation "Missing 'name'"
            Just name -> return $ Text.decodeUtf8Lenient name
    tmsMetadata <-
        getTokenState "metadata" >>= \case
            Nothing -> pltError $ QTEInvariantViolation "Missing 'metadata'"
            Just metadata ->
                either (corruptDataError "metadata url") return $ tokenMetadataUrlFromBytes $ LBS.fromStrict metadata
    tmsGovernanceAccount <-
        getTokenState "governanceAccount" >>= \case
            Nothing -> pltError $ QTEInvariantViolation "Missing governance account"
            Just govAccount -> do
                either (corruptDataError "governance account address") return $
                    deserializeFromBytes decodeAccountAddress $
                        LBS.fromStrict govAccount
    tmsAllowList <- Just . isJust <$> getTokenState "allowList"
    tmsDenyList <- Just . isJust <$> getTokenState "denyList"
    tmsMintable <- Just . isJust <$> getTokenState "mintable"
    tmsBurnable <- Just . isJust <$> getTokenState "burnable"
    let tmsAdditional = Map.empty
    return $ tokenModuleStateToBytes TokenModuleState{..}
  where
    corruptDataError name reason =
        pltError $ QTEInvariantViolation $ "Corrupt " ++ name ++ ": " ++ reason

-- | Get the CBOR-encoded representation of the token module account state.
queryAccountState :: (PLTKernelQuery m, Monad m) => PLTAccount m -> m (Maybe BS.ByteString)
queryAccountState account = do
    allowListEnabled <- isJust <$> getTokenState "allowList"
    tmasAllowList <-
        if allowListEnabled
            then Just . isJust <$> getAccountState account "allowList"
            else return Nothing
    denyListEnabled <- isJust <$> getTokenState "denyList"
    tmasDenyList <-
        if denyListEnabled
            then Just . isJust <$> getAccountState account "denyList"
            else return Nothing
    let tmasAdditional = Map.empty
    return $ Just $ tokenModuleAccountStateToBytes TokenModuleAccountState{..}
