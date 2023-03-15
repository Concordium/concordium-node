{-# LANGUAGE OverloadedStrings #-}

module Concordium.Types.BakerIdentity where

import Control.Monad
import Data.Aeson (FromJSON, parseJSON, withObject, (.:))

import qualified Concordium.Crypto.BlockSignature as Sig
import qualified Concordium.Crypto.BlsSignature as BLS
import qualified Concordium.Crypto.VRF as VRF
import Concordium.Types (
    BakerAggregationPrivateKey,
    BakerAggregationVerifyKey,
    BakerElectionPrivateKey,
    BakerElectionVerifyKey,
    BakerId,
    BakerSignPrivateKey,
    BakerSignVerifyKey,
 )

data BakerIdentity = BakerIdentity
    { bakerId :: BakerId,
      bakerSignKey :: BakerSignPrivateKey,
      bakerElectionKey :: BakerElectionPrivateKey,
      bakerAggregationKey :: BakerAggregationPrivateKey,
      bakerAggregationPublicKey :: BakerAggregationVerifyKey
    }
    deriving (Eq)

bakerSignPublicKey :: BakerIdentity -> BakerSignVerifyKey
bakerSignPublicKey ident = Sig.verifyKey (bakerSignKey ident)

bakerElectionPublicKey :: BakerIdentity -> BakerElectionVerifyKey
bakerElectionPublicKey ident = VRF.publicKey (bakerElectionKey ident)

instance FromJSON BakerIdentity where
    parseJSON v = flip (withObject "Baker identity:") v $ \obj -> do
        bakerId <- obj .: "bakerId"
        bakerSignKey <- parseJSON v
        bakerElectionKey <- parseJSON v
        bakerAggregationKey <- obj .: "aggregationSignKey"
        bakerAggregationPublicKey <- obj .: "aggregationVerifyKey"
        when (bakerAggregationPublicKey /= BLS.derivePublicKey bakerAggregationKey) $
            fail "Aggregation signing key does not correspond to the verification key."
        return BakerIdentity{..}
