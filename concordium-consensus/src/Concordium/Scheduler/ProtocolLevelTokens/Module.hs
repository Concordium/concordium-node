{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Concordium.Scheduler.ProtocolLevelTokens.Module where

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS.Builder
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word

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
    deriving (Eq)

instance Show InitializeTokenError where
    show (ITEDeserializationFailure reason) =
        "Token initialization parameters could not be deserialized: " ++ reason
    show (ITEInvalidMintAmount reason) =
        "The initial mint amount was not valid: " ++ reason

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
            setTokenState "metadata" (Just $ Text.encodeUtf8 tipMetadata)
            when tipAllowList $ setTokenState "allowList" (Just "")
            when tipDenyList $ setTokenState "denyList" (Just "")
            when tipMintable $ setTokenState "mintable" (Just "")
            when tipBurnable $ setTokenState "burnable" (Just "")
            forM_ tipInitialSupply $ \initSupply -> do
                decimals <- getDecimals
                case toTokenRawAmount decimals initSupply of
                    Left reason -> pltError (ITEInvalidMintAmount reason)
                    Right amt -> do
                        govAccount <- getGovernanceAccount
                        mintOK <- mint govAccount amt
                        unless mintOK $ pltError (ITEInvalidMintAmount "Kernel failed to mint")
  where
    tokenParamLBS =
        BS.Builder.toLazyByteString $ BS.Builder.shortByteString $ parameterBytes tokenParam

-- | A pre-processed token-holder operation. This has the transfer amount converted to a
--  'TokenRawAmount' and unwraps the metadata associated with the recipient and memo.
data PreprocessedTokenHolderOperation = PTHOTransfer
    { -- | The raw amount to transfer.
      pthoAmount :: !TokenRawAmount,
      -- | The recipient account address.
      pthoRecipient :: !TokenHolder,
      -- | The (optional) memo.
      pthoMemo :: !(Maybe Memo),
      -- | The original, unprocessed, 'TokenTransferBody'.
      pthoUnprocessed :: !TokenTransferBody
    }

-- | Convert 'TokenAmount's to 'TokenRawAmount's, checking that they are within the representable
--  range.
preprocessTokenHolderTransaction ::
    (PLTKernelFail EncodedTokenRejectReason m, Monad m) =>
    Word8 ->
    TokenHolderTransaction ->
    m (Seq.Seq PreprocessedTokenHolderOperation)
preprocessTokenHolderTransaction decimals = mapM preproc . tokenHolderTransactions
  where
    preproc (TokenHolderTransfer ttb@(TokenTransferBody{..})) =
        case toTokenRawAmount decimals ttAmount of
            Left err ->
                pltError . encodeTokenRejectReason . DeserializationFailure . Just . Text.pack $
                    "Token amount outside representable range: " ++ err
            Right pthoAmount -> return PTHOTransfer{..}
              where
                pthoRecipient = ttRecipient
                pthoMemo = taggableMemoInner <$> ttMemo
                pthoUnprocessed = ttb

-- | Execute a token-holder transaction. The process is as follows:
--
--   - Decode the transaction CBOR parameter.
--   - Check that amounts are within the representable range.
--   - For each transfer operation:
--
--       - Check that the recipient is valid.
--       - Transfer the amount from the sender to the recipient, if the sender's balance is
--         sufficient.
executeTokenHolderTransaction ::
    (PLTKernelUpdate m, PLTKernelFail EncodedTokenRejectReason m, Monad m) =>
    TransactionContext m ->
    TokenParameter ->
    m ()
executeTokenHolderTransaction TransactionContext{..} tokenParam = do
    case tokenHolderTransactionFromBytes tokenParamLBS of
        Left failureReason -> failTH $ DeserializationFailure $ Just $ Text.pack failureReason
        Right parsedTransaction -> do
            decimals <- getDecimals
            operations <- preprocessTokenHolderTransaction decimals parsedTransaction
            let handleOperation !opIndex PTHOTransfer{..} = do
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
                    success <- transfer tcSender recipientAccount pthoAmount pthoMemo
                    unless success $ do
                        availableBalance <- getAccountBalance tcSender
                        failTH
                            TokenBalanceInsufficient
                                { trrOperationIndex = opIndex,
                                  trrAvailableBalance = toTokenAmount decimals availableBalance,
                                  trrRequiredBalance = ttAmount pthoUnprocessed
                                }
                    return (opIndex + 1)
            foldM_ handleOperation 0 operations
  where
    tokenParamLBS =
        BS.Builder.toLazyByteString $ BS.Builder.shortByteString $ parameterBytes tokenParam
    failTH = pltError . encodeTokenRejectReason

-- | A pre-processed token-governance operation. This has all amounts converted to
--  'TokenRawAmount's and unwraps the metadata associated with target accounts.
data PreprocessedTokenGovernanceOperation
    = PTGOTokenMint
        {ptgoAmount :: !TokenRawAmount, ptgoUnprocessedAmount :: !TokenAmount}
    | PTGOTokenBurn
        {ptgoAmount :: !TokenRawAmount, ptgoUnprocessedAmount :: !TokenAmount}
    | PTGOTokenAddAllowList
        {ptgoTarget :: !TokenHolder}
    | PTGOTokenRemoveAllowList
        {ptgoTarget :: !TokenHolder}
    | PTGOTokenAddDenyList
        {ptgoTarget :: !TokenHolder}
    | PTGOTokenRemoveDenyList
        {ptgoTarget :: !TokenHolder}
    deriving (Eq, Show)

preprocessTokenGovernanceTransaction ::
    (PLTKernelFail EncodedTokenRejectReason m, Monad m) =>
    Word8 ->
    TokenGovernanceTransaction ->
    m (Seq.Seq PreprocessedTokenGovernanceOperation)
preprocessTokenGovernanceTransaction decimals = mapM preproc . tokenGovernanceOperations
  where
    preproc (TokenMint amount) =
        case toTokenRawAmount decimals amount of
            Left err ->
                pltError . encodeTokenRejectReason . DeserializationFailure . Just . Text.pack $
                    "Token mint amount outside representable range: " ++ err
            Right ptgoAmount -> return PTGOTokenMint{..}
      where
        ptgoUnprocessedAmount = amount
    preproc (TokenBurn amount) =
        case toTokenRawAmount decimals amount of
            Left err ->
                pltError . encodeTokenRejectReason . DeserializationFailure . Just . Text.pack $
                    "Token burn amount outside representable range: " ++ err
            Right ptgoAmount -> return PTGOTokenBurn{..}
      where
        ptgoUnprocessedAmount = amount
    preproc (TokenAddAllowList receiver) =
        return PTGOTokenAddAllowList{ptgoTarget = receiver}
    preproc (TokenRemoveAllowList receiver) =
        return PTGOTokenRemoveAllowList{ptgoTarget = receiver}
    preproc (TokenAddDenyList receiver) =
        return PTGOTokenAddDenyList{ptgoTarget = receiver}
    preproc (TokenRemoveDenyList receiver) =
        return PTGOTokenRemoveDenyList{ptgoTarget = receiver}

-- | Encode and log a 'TokenEvent'.
logEncodeTokenEvent :: (PLTKernelUpdate m) => TokenEvent -> m ()
logEncodeTokenEvent te = logTokenEvent eventType details
  where
    EncodedTokenEvent{eteType = eventType, eteDetails = details} = encodeTokenEvent te

-- | Execute a token-governance transaction. The process is as follows:
--
--   - Decode the transaction CBOR parameter.
--   - Check that amounts are within the representable range.
executeTokenGovernanceTransaction ::
    (PLTKernelPrivilegedUpdate m, PLTKernelFail EncodedTokenRejectReason m, Monad m) =>
    TransactionContext m ->
    TokenParameter ->
    m ()
executeTokenGovernanceTransaction TransactionContext{..} tokenParam = do
    case tokenGovernanceTransactionFromBytes tokenParamLBS of
        Left failureReason -> failTH $ DeserializationFailure $ Just $ Text.pack failureReason
        Right parsedTransaction -> do
            decimals <- getDecimals
            operations <- preprocessTokenGovernanceTransaction decimals parsedTransaction
            let handleOperation !opIndex op = do
                    case op of
                        PTGOTokenMint{..} -> do
                            requireFeature opIndex "mint" "mintable"
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
                        PTGOTokenBurn{..} -> do
                            requireFeature opIndex "burn" "burnable"
                            burnOK <- burn tcSender ptgoAmount
                            unless burnOK $ do
                                availableBalance <- getAccountBalance tcSender
                                failTH
                                    TokenBalanceInsufficient
                                        { trrOperationIndex = opIndex,
                                          trrAvailableBalance = toTokenAmount decimals availableBalance,
                                          trrRequiredBalance = ptgoUnprocessedAmount
                                        }
                        PTGOTokenAddAllowList{..} -> do
                            requireFeature opIndex "addAllowList" "allowList"
                            account <- requireAccount opIndex ptgoTarget
                            setAccountState account "allowList" (Just "")
                            logEncodeTokenEvent (AddAllowListEvent ptgoTarget)
                        PTGOTokenRemoveAllowList{..} -> do
                            requireFeature opIndex "removeAllowList" "allowList"
                            account <- requireAccount opIndex ptgoTarget
                            setAccountState account "allowList" Nothing
                            logEncodeTokenEvent (RemoveAllowListEvent ptgoTarget)
                        PTGOTokenAddDenyList{..} -> do
                            requireFeature opIndex "addDenyList" "denyList"
                            account <- requireAccount opIndex ptgoTarget
                            setAccountState account "denyList" (Just "")
                            logEncodeTokenEvent (AddDenyListEvent ptgoTarget)
                        PTGOTokenRemoveDenyList{..} -> do
                            requireFeature opIndex "removeDenyList" "denyList"
                            account <- requireAccount opIndex ptgoTarget
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
            Just metadata -> return $ Text.decodeUtf8Lenient metadata
    tmsAllowList <- Just . isJust <$> getTokenState "allowList"
    tmsDenyList <- Just . isJust <$> getTokenState "denyList"
    tmsMintable <- Just . isJust <$> getTokenState "mintable"
    tmsBurnable <- Just . isJust <$> getTokenState "burnable"
    let tmsAdditional = Map.empty
    return $ tokenModuleStateToBytes TokenModuleState{..}

queryAccountListStatus :: (PLTKernelQuery m, Monad m) => PLTAccount m -> m (Maybe Bool, Maybe Bool)
queryAccountListStatus account = do
    allowListEnabled <- isJust <$> getTokenState "allowList"
    isAllowed <-
        if allowListEnabled
            then Just . isJust <$> getAccountState account "allowList"
            else return Nothing
    denyListEnabled <- isJust <$> getTokenState "denyList"
    isDenied <-
        if denyListEnabled
            then Just . isJust <$> getAccountState account "denyList"
            else return Nothing
    return (isAllowed, isDenied)
