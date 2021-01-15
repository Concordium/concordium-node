{-# LANGUAGE OverloadedStrings #-}
module Concordium.GlobalState.IdentityProviders(
  module Concordium.GlobalState.IdentityProviders,
  IpInfo, ipIdentity
  )where

import qualified Data.Map.Strict as Map
import qualified Data.Serialize as S
import Data.Aeson hiding (encode, decode)

import Concordium.ID.Types
import Concordium.ID.IdentityProvider
import Concordium.Types.HashableTo
import qualified Concordium.Crypto.SHA256 as H
import Concordium.Utils.Serialization

-- |The set of all identity providers. Identity providers are identified
-- uniquely by their public key (the key used to verify signatures).
newtype IdentityProviders = IdentityProviders {
  idProviders :: Map.Map IdentityProviderIdentity IpInfo
  }
  deriving(Eq, ToJSON, FromJSON)

instance Show IdentityProviders where
    show (IdentityProviders m) = "IdentityProviders {\n" ++ concatMap f (Map.elems m) ++ "}"
        where
            f x = show x ++ "\n"

instance HashableTo H.Hash IdentityProviders where
  getHash = H.hash . S.encode

instance Monad m => MHashableTo m H.Hash IdentityProviders where

emptyIdentityProviders :: IdentityProviders
emptyIdentityProviders = IdentityProviders Map.empty

instance S.Serialize IdentityProviders where
    put IdentityProviders{..} =
      let l = Map.size idProviders
      in S.putWord32be (fromIntegral l) <> putSafeSizedMapOf S.put S.put idProviders
    get = do
      l <- S.getWord32be
      IdentityProviders <$> getSafeSizedMapOf l S.get S.get
