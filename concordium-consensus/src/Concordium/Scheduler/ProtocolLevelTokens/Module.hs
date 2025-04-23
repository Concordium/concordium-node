{-# LANGUAGE OverloadedStrings #-}

module Concordium.Scheduler.ProtocolLevelTokens.Module where

import Control.Monad
import qualified Data.ByteString.Builder as BS.Builder
import qualified Data.Text.Encoding as Text

import Concordium.Types
import Concordium.Types.ProtocolLevelTokens.CBOR
import Concordium.Types.Tokens

import Concordium.Scheduler.ProtocolLevelTokens.Kernel

data InitializeTokenError
    = ITEDeserializationFailure !String
    | ITEInvalidMintAmount
    deriving (Eq, Show)

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
