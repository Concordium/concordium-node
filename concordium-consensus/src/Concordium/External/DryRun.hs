module Concordium.External.DryRun where

import Foreign

import qualified Concordium.External as Ext
import Concordium.Logger
import Concordium.MultiVersion

-- |An opaque rust vector.
data ForeignVec

type CopyToForeignVec = Ptr ForeignVec -> Ptr Word8 -> Int64 -> IO ()

-- |Boilerplate wrapper to invoke C callbacks.
foreign import ccall "dynamic" callCopyToForeignVecCallback :: FunPtr CopyToForeignVec -> CopyToForeignVec

data DryRunHandle = DryRunHandle
    { drhConsensus :: !Ext.ConsensusRunner,
      drhWriteToVector :: !CopyToForeignVec
    }

dryRunStart :: StablePtr Ext.ConsensusRunner -> FunPtr CopyToForeignVec -> IO (StablePtr DryRunHandle)
dryRunStart consensusPtr vecCallback = do
    consensus@(Ext.ConsensusRunner mvr) <- deRefStablePtr consensusPtr
    dryRunPtr <-
        newStablePtr $!
            DryRunHandle
                { drhConsensus = consensus,
                  drhWriteToVector = callCopyToForeignVecCallback vecCallback
                }
    mvLog mvr External LLTrace $ "Dry run start " ++ show (castStablePtrToPtr dryRunPtr)
    return dryRunPtr

dryRunEnd :: StablePtr DryRunHandle -> IO ()
dryRunEnd dryRunPtr = do
    DryRunHandle{drhConsensus = Ext.ConsensusRunner mvr} <- deRefStablePtr dryRunPtr
    mvLog mvr External LLTrace $ "Dry run end " ++ show (castStablePtrToPtr dryRunPtr)
    freeStablePtr dryRunPtr

foreign export ccall
    dryRunStart ::
        StablePtr Ext.ConsensusRunner ->
        FunPtr CopyToForeignVec ->
        IO (StablePtr DryRunHandle)

foreign export ccall
    dryRunEnd :: StablePtr DryRunHandle -> IO ()
