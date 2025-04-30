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
      pthoRecipient :: !AccountAddress,
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
                pltError . encodeTokenHolderFailure . DeserializationFailure . Just . Text.pack $
                    "Token amount outside representable range: " ++ err
            Right pthoAmount -> return PTHOTransfer{..}
              where
                pthoRecipient = receiverAccountAddress ttRecipient
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
                    getAccount pthoRecipient >>= \case
                        Nothing ->
                            failTH
                                RecipientNotFound
                                    { thfOperationIndex = opIndex,
                                      thfRecipient = ttRecipient pthoUnprocessed
                                    }
                        Just recipientAccount -> do
                            success <- transfer sender recipientAccount pthoAmount pthoMemo
                            unless success $ do
                                availableBalance <- fromMaybe 0 <$> getAccountBalance sender
                                failTH
                                    TokenBalanceInsufficient
                                        { thfOperationIndex = opIndex,
                                          thfAvailableBalance = toTokenAmount decimals availableBalance,
                                          thfRequiredBalance = ttAmount pthoUnprocessed
                                        }
                            return (opIndex + 1)
            foldM_ handleOperation 0 operations
  where
    tokenParamLBS =
        BS.Builder.toLazyByteString $ BS.Builder.shortByteString $ parameterBytes tokenParam
    failTH = pltError . encodeTokenHolderFailure
