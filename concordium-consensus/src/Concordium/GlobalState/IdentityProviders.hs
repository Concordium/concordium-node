{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Concordium.GlobalState.IdentityProviders(
  module Concordium.GlobalState.IdentityProviders,
  IpInfo, ipIdentity
  )where

import Data.HashMap.Strict(HashMap)
import qualified Data.HashMap.Strict as HM

import Concordium.ID.Types
import Concordium.ID.IdentityProvider

-- |The set of all identity providers. Identity providers are identified
-- uniquely by their public key (the key used to verify signatures).
newtype IdentityProviders = IdentityProviders {
  idProviders :: HashMap IdentityProviderIdentity IpInfo
  }

instance Show IdentityProviders where
    show (IdentityProviders m) = "IdentityProviers {\n" ++ concatMap f (HM.elems m) ++ "}"
        where
            f x = show x ++ "\n"

emptyIdentityProviders :: IdentityProviders
emptyIdentityProviders = IdentityProviders HM.empty
