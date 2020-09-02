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
import Control.Monad (replicateM)

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
    put IdentityProviders{..} = do
      let l = Map.toAscList idProviders
      S.putWord32be (fromIntegral $ length l)
      mapM_ S.put l
    get = do
      l <- S.getWord32be
      ascList <- replicateM (fromIntegral l) S.get
      return . IdentityProviders . Map.fromAscList $ ascList -- TODO js: once on top of Thomas' changes see https://gitlab.com/Concordium/consensus/globalstate-mockup/-/merge_requests/110#note_397883944
