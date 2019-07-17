{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Concordium.GlobalState.IdentityProviders where

import Data.HashMap.Strict(HashMap)
import qualified Data.HashMap.Strict as HM

import Concordium.ID.Types
import Concordium.Crypto.FFIDataTypes

import qualified Data.Serialize as S

import Data.Aeson
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16

data AnonymityRevokerData = AnonymityRevokerData {
  -- |Unique id of the anonymity revoker.
  arIdentity :: !ARName,
  -- |Public key of the anonymity revoker
  arPublicKey :: !AnonymityRevokerPublicKey,
  -- |Generator of the group the anonymity revoker uses
  -- FIXME: This is really part of the public key but currently
  -- this is not the abstraction in crypto.
  arElgamalGenerator :: !ElgamalGen
  } deriving(Show)

instance S.Serialize AnonymityRevokerData where
  put AnonymityRevokerData{..} =
    S.put arIdentity <> S.put arPublicKey <> S.put arElgamalGenerator

  get = do
    arIdentity <- S.get
    arPublicKey <- S.get
    arElgamalGenerator <- S.get
    return AnonymityRevokerData{..}

-- |Data needed by the bakers to reward identity providers and keep track of any
-- other necessary things.
-- TODO: Any other data we need, perhaps country, jurisdiction, ...
data IdentityProviderData = IdentityProviderData {
  -- |Unique identity of the identity provider.
  ipIdentity :: !IdentityProviderIdentity,
  -- |Public key of the identity provider. Can change throughout the lifetime.
  ipVerifyKey :: !IdentityProviderPublicKey,
  -- |Account of the identity provider. Each identity provider must designate an
  -- account to which it will receive rewards. This can change throughout the
  -- lifetime.
  -- FIXME: Commented out for POC.
  -- ipAccount :: !AccountAddress,
  -- |Associated anonymity revoker of this identity provider.
  ipArInfo :: AnonymityRevokerData
  } deriving (Show)

-- |The set of all identity providers. Identity providers are identified
-- uniquely by their public key (the key used to verify signatures).
newtype IdentityProviders = IdentityProviders {
  idProviders :: HashMap IdentityProviderIdentity IdentityProviderData
  }

instance Show IdentityProviders where
    show (IdentityProviders m) = "IdentityProviers {\n" ++ concatMap f (HM.elems m) ++ "}"
        where
            f x = show x ++ "\n"

emptyIdentityProviders :: IdentityProviders
emptyIdentityProviders = IdentityProviders HM.empty


instance FromJSON IdentityProviderData where
  parseJSON = withObject "IdentityProviderData" $ \v ->
    do ipIdbs <- Text.encodeUtf8 <$> (v .: "ipIdentity")
       ipVerifyKeybs <- b16 <$> (v .: "ipVerifyKey")
       arPublicKeybs <- b16 <$> (v .: "arPublicKey")
       arName <- Text.encodeUtf8 <$> (v .: "arName")
       arElgamalGeneratorbs <- b16 <$> (v .: "arElgamalGenerator")
       -- we need to add length information to the deserializer for this field.
       -- This is an unfortunate hack, but ...
       let ipVerifyKey = S.decode (lenbs (ipVerifyKeybs))
       let arPublicKey' = S.decode arPublicKeybs
       let arElgamalGenerator' = S.decode arElgamalGeneratorbs
       case (ipVerifyKey, arPublicKey', arElgamalGenerator') of
         (Right iver, Right arPublicKey, Right arElgamalGenerator) -> 
           return IdentityProviderData{
           ipIdentity = IP_ID ipIdbs,
           ipVerifyKey = IP_PK iver,
           ipArInfo = AnonymityRevokerData{
               arIdentity = ARName arName,
               ..
               }
           }
         _ -> fail "Could not decode keys."
           
         where b16 = fst . BS16.decode . Text.encodeUtf8
               lenbs bs = S.runPut (S.putWord32be (fromIntegral (BS.length bs))) <> bs

instance S.Serialize IdentityProviderData where
  put IdentityProviderData{..} =
    S.put ipIdentity <>
    S.put ipVerifyKey <>
    S.put ipArInfo

  get = do
    ipIdentity <- S.get
    ipVerifyKey <- S.get
    ipArInfo <- S.get
    return IdentityProviderData{..}
