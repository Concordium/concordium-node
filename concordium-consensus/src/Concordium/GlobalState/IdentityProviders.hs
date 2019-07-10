{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Concordium.GlobalState.IdentityProviders where

import Data.HashMap.Strict(HashMap)
import qualified Data.HashMap.Strict as HM

import Concordium.Types
import Concordium.ID.Types

import qualified Data.Serialize as S

import Data.Aeson
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Base16 as BS16

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
  ipAccount :: !AccountAddress
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
    do ipIdText <- v .: "ipId"
       ipVerifyKeyText <- v .: "ipVerifykey"
       let ipIdbs = fst . BS16.decode . Text.encodeUtf8 $ ipIdText
       let ipVerifyKeybs = fst . BS16.decode . Text.encodeUtf8 $ ipVerifyKeyText
       ipAccountText <- v .: "ipAccount" -- assume base16 for now to simplify things
       case S.decode (fst . BS16.decode . Text.encodeUtf8 $ ipAccountText) of
         Left err -> fail $ "Cannot read account address: " ++ err
         Right ipAccount ->
           return IdentityProviderData{
           ipIdentity = IP_ID ipIdbs,
           ipVerifyKey = IP_PK ipVerifyKeybs,
           ..
           }

instance S.Serialize IdentityProviderData where
  put IdentityProviderData{..} =
    S.put ipIdentity <>
    S.put ipVerifyKey <>
    S.put ipAccount

  get = do
    ipIdentity <- S.get
    ipVerifyKey <- S.get
    ipAccount <- S.get
    return IdentityProviderData{..}
