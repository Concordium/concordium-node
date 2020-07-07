{-# LANGUAGE DerivingVia #-}
module Concordium.GlobalState.AnonymityRevokers(
  module Concordium.GlobalState.AnonymityRevokers,
  ArInfo, arIdentity
  )where

import Data.HashMap.Strict(HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Serialize as S
import Data.Aeson(FromJSON, ToJSON)

import Concordium.ID.Types
import Concordium.ID.AnonymityRevoker

-- |The set of all anonymity revokers. Anonymity revokers are identified
-- uniquely by their identity. The public key of an anonymity revoker should
-- never be updated. A new one should be added if that is needed.
newtype AnonymityRevokers = AnonymityRevokers {
  arRevokers :: HashMap ArIdentity ArInfo
  }
  deriving(Eq)
  deriving(FromJSON, ToJSON) via (HashMap ArIdentity ArInfo)

instance Show AnonymityRevokers where
    show (AnonymityRevokers m) = "AnonymityRevokers {\n" ++ concatMap f (HM.elems m) ++ "}"
        where
            f x = show x ++ "\n"

emptyAnonymityRevokers :: AnonymityRevokers
emptyAnonymityRevokers = AnonymityRevokers HM.empty

instance S.Serialize AnonymityRevokers where
    put AnonymityRevokers{..} = S.put (HM.toList arRevokers)
    get = AnonymityRevokers . HM.fromList <$> S.get
