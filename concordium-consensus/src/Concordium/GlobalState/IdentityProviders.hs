{-# LANGUAGE OverloadedStrings #-}
module Concordium.GlobalState.IdentityProviders(
  module Concordium.GlobalState.IdentityProviders,
  IpInfo, ipIdentity
  )where

import Data.HashMap.Strict(HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Serialize as S
import Data.Aeson hiding (encode, decode)

import Concordium.ID.Types
import Concordium.ID.IdentityProvider

-- |The set of all identity providers. Identity providers are identified
-- uniquely by their public key (the key used to verify signatures).
newtype IdentityProviders = IdentityProviders {
  idProviders :: HashMap IdentityProviderIdentity IpInfo
  }
  deriving(Eq, ToJSON, FromJSON)

instance Show IdentityProviders where
    show (IdentityProviders m) = "IdentityProviders {\n" ++ concatMap f (HM.elems m) ++ "}"
        where
            f x = show x ++ "\n"

emptyIdentityProviders :: IdentityProviders
emptyIdentityProviders = IdentityProviders HM.empty

instance S.Serialize IdentityProviders where
    put IdentityProviders{..} = S.put (HM.toList idProviders)
    get = IdentityProviders . HM.fromList <$> S.get
