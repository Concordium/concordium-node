{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module BlockStateDump.StateDump.ProtocolLevelTokens where

import Data.Coerce

import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Types
import qualified Control.Monad.RWS as RWST

import qualified BlockStateDump.StateDump.LFMBTree as LFMBDump
import qualified Concordium.GlobalState.ContractStateV1 as PST
import qualified Concordium.GlobalState.Persistent.BlobStore as Blob
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import qualified Concordium.GlobalState.Persistent.BlockState.ProtocolLevelTokens as PLT
import qualified Concordium.GlobalState.Persistent.BlockState.ProtocolLevelTokens.RustPLTBlockState as PLT
import qualified Concordium.Types.Tokens as PLT

import BlockStateDump.Shared
import Control.Monad.State (lift)

dumpProtocolLevelTokens ::
    forall pv m.
    (BS.SupportsPersistentState pv m) =>
    NodeId ->
    PLT.ProtocolLevelTokensForStateVersion (PltStateVersionFor pv) ->
    StateDumpMonad m ()
dumpProtocolLevelTokens parentNode pltsForSV = do
    case pltsForSV of
        PLT.ProtocolLevelTokensNone -> return ()
        PLT.ProtocolLevelTokensV0 pltsStateRef -> do
            visitHBRNode parentNode "plts" "plts" pltsStateRef $ \pltsStateNode _ _ -> do
                pltsState <- lift $ Blob.refLoad pltsStateRef
                LFMBDump.dumpLFMBTree "plttbl" pltsStateNode (PLT._pltTable pltsState) $ \pltLeafNode pltRef -> do
                    -- todo ar key value state
                    plt <- lift $ Blob.refLoad pltRef
                    let pltConfRef = PLT._pltConfiguration plt
                    pltConf <- lift $ Blob.refLoad pltConfRef

                    visitHBRNode pltLeafNode "" (show $ show $ PLT._pltTokenId pltConf) pltRef $ \pltNode pltBlobRef pltHash -> do
                        let plt' =
                                PLT'
                                    { _pltCirculatingSupply = PLT._pltCirculatingSupply plt
                                    }
                        buildStateData (coerce pltBlobRef) pltHash plt'
                        visitHBRNode pltNode "" "pltconf" pltConfRef $ \_pltConfNode pltConfBlobRef pltConfHash -> do
                            buildStateData pltConfBlobRef pltConfHash pltConf
                        let NodeId pltNodeWord = pltNode
                        OutputFilesPaths{..} <- RWST.ask
                        closeOutputFiles
                        lift $ PST.dumpPersistentState (PLT._pltState plt) pltNodeWord ofpStateGraphFilePath ofpStateFilePath
                        reopenOutputFiles                        
        PLT.ProtocolLevelTokensV1 pltsState -> do
            let NodeId parentNodeWord = parentNode
            OutputFilesPaths{..} <- RWST.ask
            closeOutputFiles
            lift $ PLT.dumpPLTBlockState pltsState parentNodeWord ofpStateGraphFilePath ofpStateFilePath
            reopenOutputFiles

data PLT' = PLT'
    { _pltCirculatingSupply :: !PLT.TokenRawAmount
    }
    deriving (Show)
