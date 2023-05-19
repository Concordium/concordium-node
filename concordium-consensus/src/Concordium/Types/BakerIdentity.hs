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

-- |The identity of a baker.
data BakerIdentity = BakerIdentity
    { -- |The id of the baker.
      -- Note that this is equivalent to the 'AccountIndex' of the
      -- account registered as being a baker.
      bakerId :: BakerId,
      -- |A private EDDSA key used for signing a block that the
      -- baker has baked.
      bakerSignKey :: BakerSignPrivateKey,
      -- |A private VRF key used for checking leadership.
      bakerElectionKey :: BakerElectionPrivateKey,
      -- |A private BLS key for creating aggregate signatures used by
      -- the finalization layer.
      bakerAggregationKey :: BakerAggregationPrivateKey,
      -- |The public key that corresponds to 'bakerAggregationKey' and
      -- should be used for verifying BLS signatures that stems from
      -- this baker.
      bakerAggregationPublicKey :: BakerAggregationVerifyKey
    }
    deriving (Eq)

-- |Retrieve the public key corresponding to the private EDDSA key of the 'BakerIdentity'.
-- This is used the check the authenticity of the baker.
bakerSignPublicKey :: BakerIdentity -> BakerSignVerifyKey
bakerSignPublicKey ident = Sig.verifyKey (bakerSignKey ident)

-- |Retrieve the public key corresponding to the private VRF key of the 'BakerIdentity'.
-- This is used to check whether a 'BlockProof' is valid or not.
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
