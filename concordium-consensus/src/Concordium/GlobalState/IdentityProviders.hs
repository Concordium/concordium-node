module Concordium.GlobalState.IdentityProviders where

import Data.HashMap.Strict(HashMap)
import qualified Data.HashMap.Strict as HM

import Concordium.Types
import Concordium.ID.Types

-- |Data needed by the bakers to reward identity providers and keep track of any
-- other necessary things.
-- TODO: Any other data we need, perhaps country, jurisdiction, ...
data IdentityProviderData = IdentityProviderData {
  -- |Unique identity of the identity provider.
  idIdentity :: !IdentityProviderIdentity,
  -- |Public key of the identity provider. Can change throughout the lifetime.
  idPubKey :: !IdentityProviderPublicKey,
  -- |Account of the identity provider. Each identity provider must designate an
  -- account to which it will receive rewards. This can change throughout the
  -- lifetime.
  idAccount :: !AccountAddress
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
