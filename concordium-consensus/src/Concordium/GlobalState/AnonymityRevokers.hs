{-# LANGUAGE DerivingVia #-}
module Concordium.GlobalState.AnonymityRevokers(
  module Concordium.GlobalState.AnonymityRevokers,
  ArInfo, arIdentity
  )where

import qualified Data.Map.Strict as Map
import qualified Data.Serialize as S
import Data.Aeson(FromJSON, ToJSON)

import Concordium.ID.Types
import Concordium.ID.AnonymityRevoker
import Concordium.Types.HashableTo
import qualified Concordium.Crypto.SHA256 as H
import Control.Monad (replicateM)

-- |The set of all anonymity revokers. Anonymity revokers are identified
-- uniquely by their identity. The public key of an anonymity revoker should
-- never be updated. A new one should be added if that is needed.
newtype AnonymityRevokers = AnonymityRevokers {
  arRevokers :: Map.Map ArIdentity ArInfo
  }
  deriving(Eq)
  deriving(FromJSON, ToJSON) via (Map.Map ArIdentity ArInfo)

instance Show AnonymityRevokers where
    show (AnonymityRevokers m) = "AnonymityRevokers {\n" ++ concatMap f (Map.elems m) ++ "}"
        where
            f x = show x ++ "\n"

instance HashableTo H.Hash AnonymityRevokers where
  getHash = H.hash . S.encode

instance Monad m => MHashableTo m H.Hash AnonymityRevokers where

emptyAnonymityRevokers :: AnonymityRevokers
emptyAnonymityRevokers = AnonymityRevokers Map.empty

instance S.Serialize AnonymityRevokers where
    put AnonymityRevokers{..} = do
      let l = Map.toAscList arRevokers
      S.putWord32be (fromIntegral $ length l)
      mapM_ S.put l
    get = do
      l <- S.getWord32be
      ascList <- replicateM (fromIntegral l) S.get
      return . AnonymityRevokers . Map.fromAscList $ ascList -- TODO js: once on top of Thomas' changes see https://gitlab.com/Concordium/consensus/globalstate-mockup/-/merge_requests/110#note_397883944
