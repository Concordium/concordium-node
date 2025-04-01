module Concordium.GlobalState.Persistent.Tokens where

import Data.Word
import Concordium.GlobalState.Persistent.CachedRef
import Concordium.GlobalState.Persistent.Cache
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.LFMBTree (LFMBTree')
import Concordium.ID.Types
import Concordium.Types

data Tokens = Tokens {
  tokenTable :: !(LFMBTree' TokenIndex HashedBufferedRef TokenRef)
}

type TokenRef = HashedCachedRef TokenCache PersistentToken
type TokenCache = FIFOCache PersistentToken

data PersistentToken = PersistentToken {
  tokenId :: TokenId,
  tokenModuleRef :: TokenModuleRef,
  tokenIssuer :: AccountAddress,
  tokenNrOfDecimals :: Word32,
  tokenTotalSupply :: TokenAmount,
  tokenModuleState :: Cbor
} 
