module Concordium.GlobalState.IdentityProviders where

import Data.HashMap.Strict(HashMap)
import qualified Data.HashMap.Strict as HM

import Data.ByteString(ByteString)

import Concordium.Types
import Concordium.ID.Types

-- |Data needed by the bakers to reward identity providers and keep track of any
-- other necessary things.
-- TODO: Any other data we need, perhaps country, jurisdiction, ...
data IdentityProviderData = IdentityProviderData {
  -- |Textual name of the identity provider, not necessarily unique.
  idName :: ByteString,
  -- |Public key of the identity provider. Uniquely identifies an identity
  -- provider.
  idPubKey :: IdentityProviderPublicKey,
  -- |Account of the identity provider. Each identity provider must designate an
  -- account to which it will receive rewards.
  idAccount :: AccountAddress
  }

-- |The set of all identity providers. Identity providers are identified
-- uniquely by their public key (the key used to verify signatures).
data IdentityProviders = IdentityProviders {
  idProviders :: HashMap IdentityProviderPublicKey IdentityProviderData
  }


emptyIdentityProviders :: IdentityProviders
emptyIdentityProviders = IdentityProviders HM.empty
