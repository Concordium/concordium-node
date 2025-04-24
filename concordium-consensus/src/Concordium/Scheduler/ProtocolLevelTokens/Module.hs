{-# LANGUAGE OverloadedStrings #-}

module Concordium.Scheduler.ProtocolLevelTokens.Module where

import Control.Monad
import qualified Data.ByteString.Builder as BS.Builder
import qualified Data.Text.Encoding as Text

import Concordium.Types
import Concordium.Types.ProtocolLevelTokens.CBOR
import Concordium.Types.Tokens

import Concordium.Scheduler.ProtocolLevelTokens.Kernel

-- | Class for constructing a 'TokenModuleRejectReason' given the 'TokenId'.
class MakeTokenModuleRejectReason e where
    -- | Construct a 'TokenModuleRejectReason'.
    makeTokenModuleRejectReason :: TokenId -> e -> TokenModuleRejectReason

-- | Represents the reasons why 'initializeToken' can fail.
data InitializeTokenError
    = ITEDeserializationFailure !String
    | ITEInvalidMintAmount
    deriving (Eq, Show)

instance MakeTokenModuleRejectReason InitializeTokenError where
    makeTokenModuleRejectReason tmrrTokenSymbol (ITEDeserializationFailure _) =
        TokenModuleRejectReason
            { tmrrType = TokenEventType "parameterDeserializationFailure",
              tmrrDetails = Nothing,
              ..
            }
    makeTokenModuleRejectReason tmrrTokenSymbol ITEInvalidMintAmount =
        TokenModuleRejectReason
            { tmrrType = TokenEventType "invalidMintAmount",
              tmrrDetails = Nothing,
              ..
            }

-- | Try to convert a 'TokenAmount' to a 'TokenRawAmount'. The latter is represented in the
--  smallest subdivision allowed for the current token.
--  This can fail if:
--
--   - The token amount specifies more decimals than the token allows in its representation.
--   - The amount would be outside of the representable range as a 'TokenRawAmount'.
toTokenRawAmount :: (PLTKernelQuery m, Monad m) => TokenAmount -> m (Maybe TokenRawAmount)
toTokenRawAmount TokenAmount{..} = do
    actualDecimals <- getDecimals
    case compare nrDecimals (fromIntegral actualDecimals) of
        EQ -> return (Just (TokenRawAmount digits))
        GT -> return Nothing
        LT -> do
            let factor = 10 ^ (fromIntegral actualDecimals - nrDecimals)
            let rawAmountInteger = factor * toInteger digits
            if rawAmountInteger > fromIntegral (maxBound :: TokenRawAmount)
                then return Nothing
                else return (Just (fromIntegral rawAmountInteger))

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
                toTokenRawAmount initSupply >>= \case
                    Nothing -> pltError ITEInvalidMintAmount
                    Just amt -> do
                        govAccount <- getGovernanceAccount
                        mintOK <- mint govAccount amt
                        unless mintOK $ pltError ITEInvalidMintAmount
  where
    tokenParamLBS =
        BS.Builder.toLazyByteString $ BS.Builder.shortByteString $ parameterBytes tokenParam
