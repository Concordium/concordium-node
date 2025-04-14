{-# LANGUAGE OverloadedStrings #-}

module Concordium.Scheduler.ProtocolLevelTokens.Module where

import Codec.CBOR.Read
import Control.Monad
import Control.Monad.Error.Class
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as Text

import Concordium.Types
import Concordium.Types.ProtocolLevelTokens.CBOR

import Concordium.Scheduler.ProtocolLevelTokens.Kernel

data InitializeTokenError
    = ITEDeserializationFailure !String
    | ITEInvalidMintAmount

toTokenRawAmount :: TokenAmount -> m (Maybe TokenRawAmount)
toTokenRawAmount TokenAmount{..} = do
    actualDecimals <- getDecimals
    case compare nrDecimals (fromIntegral actualDecimals) of
        EQ -> return (Just (TokenRawAmount digits))
        GT -> return Nothing
        LT -> do
            let factor = 10 ^ (fromIntegral actualDecimals - fromIntegral nrDecimals)
            let rawAmountInteger = factor * toInteger digits
            if rawAmountInteger > fromIntegral (maxBound :: TokenRawAmount)
                then return Nothing
                else return (fromIntegral rawAmountInteger)

initializeToken ::
    (PLTKernelPrivilegedUpdate m, MonadError InitializeTokenError m) =>
    BS.ByteString ->
    m ()
initializeToken tokenParam = do
    case deserialiseFromBytes decodeTokenInitializationParameters (LBS.fromStrict tokenParam) of
        Left failureReason -> throwError $ ITEDeserializationFailure (show failureReason)
        Right (remaining, TokenInitializationParameters{..})
            | not (LBS.null remaining) -> throwError $ ITEDeserializationFailure "Parameter was not fully consumed"
            | otherwise -> do
                setTokenState "name" (Just $ Text.encodeUtf8 tipName)
                setTokenState "metadata" (Just $ Text.encodeUtf8 tipMetadata)
                when tipAllowList $ setTokenState "allowList" (Just "")
                when tipDenyList $ setTokenState "denyList" (Just "")
                when tipMintable $ setTokenState "mintable" (Just "")
                when tipBurnable $ setTokenState "burnable" (Just "")
                forM_ tipInitialSupply $ \initSupply -> do
                    case toTokenRawAmount initSupply of
                        Nothing -> throwError ITEInvalidMintAmount
                        Just amt -> do
                            govAccount <- getGovernanceAccount
                            mint govAccount amt
