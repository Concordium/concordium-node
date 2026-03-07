{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module BlockStateDump.StateDump.ProtocolLevelTokens where

import Control.Monad
import Control.Monad.IO.Class

import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Types
import qualified Concordium.Types.HashableTo as Hash

import qualified Concordium.GlobalState.Persistent.BlobStore as Blob
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import qualified Concordium.GlobalState.Persistent.BlockState.ProtocolLevelTokens as PLT
import qualified Concordium.GlobalState.Persistent.BlockState.ProtocolLevelTokens.RustPLTBlockState as PLT

import BlockStateDump.Shared
import qualified BlockStateDump.StateDump.LFMBTree as LFMBDump
import qualified Concordium.Types.Tokens as PLT
import Data.Coerce

dumpProtocolLevelTokens ::
    forall pv m.
    (BS.SupportsPersistentState pv m) =>
    OutputFiles ->
    NodeId ->
    (PLT.ProtocolLevelTokensForStateVersion (PltStateVersionFor pv)) ->
    m ()
dumpProtocolLevelTokens output parentNode pltsForSV = do
    case pltsForSV of
        PLT.ProtocolLevelTokensNone -> return ()
        PLT.ProtocolLevelTokensV0 pltsStateRef -> do
            -- (pltsStateBlobRef, PLT.ProtocolLevelTokensHash pltsStateHash) <- getHBRRefAndHash pltsStateRef
            -- maybePltsStateNode <- liftBSOIO $ buildBlobRefNode output "plts" parentNode "plts" pltsStateBlobRef pltsStateHash

            visitHBRNode output parentNode "plts" "plts" pltsStateRef $ \pltsStateNode _ _ -> do
                pltsState <- Blob.refLoad pltsStateRef
                LFMBDump.dumpLFMBTree output "pltmap" pltsStateNode (PLT._pltTable pltsState) $ \pltLeafNode pltRef -> do
                    -- todo ar key value state
                    plt <- Blob.refLoad pltRef
                    let pltConfRef = PLT._pltConfiguration plt
                    pltConf <- Blob.refLoad pltConfRef

                    visitHBRNode output pltLeafNode "" (show $ show $ PLT._pltTokenId pltConf) pltRef $ \pltNode pltBlobRef pltHash -> do
                        let plt' =
                                PLT'
                                    { _pltCirculatingSupply = PLT._pltCirculatingSupply plt
                                    }
                        liftIO $ buildStateData output (coerce pltBlobRef) pltHash plt'
                        visitHBRNode output pltNode "" "pltconf" pltConfRef $ \_pltConfNode pltConfBlobRef pltConfHash -> do
                            liftIO $ buildStateData output pltConfBlobRef pltConfHash pltConf
        PLT.ProtocolLevelTokensV1 pltsState -> do
            return () -- todo ar

data PLT' = PLT'
    { -- | The total amount of the token that exists in circulation.
      _pltCirculatingSupply :: !PLT.TokenRawAmount
    }
    deriving (Show)
