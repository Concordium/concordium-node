{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Concordium.Scheduler.ProtocolLevelTokens.Module where

import Control.Monad
import qualified Data.ByteString.Builder as BS.Builder
import Data.Maybe
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word

import Concordium.Types
import Concordium.Types.ProtocolLevelTokens.CBOR
import Concordium.Types.Tokens

import Concordium.Scheduler.ProtocolLevelTokens.Kernel

-- | Represents the reasons why 'initializeToken' can fail.
data InitializeTokenError
    = ITEDeserializationFailure !String
    | ITEInvalidMintAmount !String
    deriving (Eq)

instance Show InitializeTokenError where
    show (ITEDeserializationFailure reason) =
        "Token initialization parameters could not be deserialized: " ++ reason
    show (ITEInvalidMintAmount reason) =
        "The initial mint amount was outside of the representable range: " ++ reason

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
toTokenRawAmount actualDecimals TokenAmount{..} =
    case compare nrDecimals (fromIntegral actualDecimals) of
        EQ -> Right (TokenRawAmount digits)
        GT -> Left "Token amount precision exceeds representable precision"
        LT
            | rawAmountInteger > fromIntegral (maxBound :: TokenRawAmount) ->
                Left "Token amount exceeds maximum representable amount"
            | otherwise -> Right (fromIntegral rawAmountInteger)
          where
            factor = 10 ^ (fromIntegral actualDecimals - nrDecimals)
            rawAmountInteger = factor * toInteger digits

-- | Convert a 'TokenRawAmount' to a 'TokenAmount' given the number of decimals in the
--  representation.
toTokenAmount ::
    -- | Number of decimals
    Word8 ->
    -- | Amount to convert
    TokenRawAmount ->
    -- | Converted amount
    TokenAmount
toTokenAmount decimals (TokenRawAmount rawAmount) =
    TokenAmount
        { digits = rawAmount,
          nrDecimals = fromIntegral decimals
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
                pthoMemo = untaggedMemo <$> ttMemo
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
    PLTAccount m ->
    TokenParameter ->
    m ()
executeTokenHolderTransaction sender tokenParam = do
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
                        senderAllowed <- isJust <$> getAccountState sender "allowList"
                        unless senderAllowed $ do
                            -- FIXME: Maybe we should have the address passed in, so we report
                            -- it consistently.
                            senderAddress <- getAccountCanonicalAddress sender
                            failTH
                                OperationNotPermitted
                                    { trrOperationIndex = opIndex,
                                      trrAddressNotPermitted =
                                        Just (accountTokenHolder senderAddress),
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
                        senderDenied <- isJust <$> getAccountState sender "denyList"
                        when senderDenied $ do
                            senderAddress <- getAccountCanonicalAddress sender
                            failTH
                                OperationNotPermitted
                                    { trrOperationIndex = opIndex,
                                      trrAddressNotPermitted =
                                        Just (accountTokenHolder senderAddress),
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
                    success <- transfer sender recipientAccount pthoAmount pthoMemo
                    unless success $ do
                        availableBalance <- getAccountBalance sender
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

-- | Execute a token-governance transaction. The process is as follows:
--
--   - Decode the transaction CBOR parameter.
--   - Check that amounts are within the representable range.
executeTokenGovernanceTransaction ::
    (PLTKernelPrivilegedUpdate m, PLTKernelFail EncodedTokenRejectReason m, Monad m) =>
    PLTAccount m ->
    TokenParameter ->
    m ()
executeTokenGovernanceTransaction sender tokenParam = do
    case tokenGovernanceTransactionFromBytes tokenParamLBS of
        Left failureReason -> failTH $ DeserializationFailure $ Just $ Text.pack failureReason
        Right parsedTransaction -> do
            decimals <- getDecimals
            operations <- preprocessTokenGovernanceTransaction decimals parsedTransaction
            let handleOperation !opIndex op = do
                    case op of
                        PTGOTokenMint{..} -> do
                            requireFeature opIndex "mint" "mintable"
                            mintOK <- mint sender ptgoAmount
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
                            burnOK <- burn sender ptgoAmount
                            unless burnOK $ do
                                availableBalance <- getAccountBalance sender
                                failTH
                                    TokenBalanceInsufficient
                                        { trrOperationIndex = opIndex,
                                          trrAvailableBalance = toTokenAmount decimals availableBalance,
                                          trrRequiredBalance = ptgoUnprocessedAmount
                                        }
                        PTGOTokenAddAllowList{..} -> do
                            requireFeature opIndex "add-allow-list" "allowList"
                            account <- requireAccount opIndex ptgoTarget
                            setAccountState account "allowList" (Just "")
                        PTGOTokenRemoveAllowList{..} -> do
                            requireFeature opIndex "remove-allow-list" "allowList"
                            account <- requireAccount opIndex ptgoTarget
                            setAccountState account "allowList" Nothing
                        PTGOTokenAddDenyList{..} -> do
                            requireFeature opIndex "add-deny-list" "denyList"
                            account <- requireAccount opIndex ptgoTarget
                            setAccountState account "denyList" (Just "")
                        PTGOTokenRemoveDenyList{..} -> do
                            requireFeature opIndex "remove-deny-list" "denyList"
                            account <- requireAccount opIndex ptgoTarget
                            setAccountState account "denyList" Nothing
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
    allowList <- getTokenState feature
    when (isNothing allowList) $
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
