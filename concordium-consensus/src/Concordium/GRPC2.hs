{- |Part of the implementation of the GRPC2 interface This module constructs
 responses to queries that are handled by the Haskell part of the code.
-}
module Concordium.GRPC2 where

import Control.Concurrent
import Control.Monad
import qualified Data.ByteString.Unsafe as BS
import Data.Int
import qualified Data.Serialize as S
import Data.Word
import Foreign

import Concordium.Types

import qualified Concordium.External as Ext -- TODO: This is not an ideal configuration.
import Concordium.MultiVersion (
    Callbacks (..),
    CatchUpConfiguration (..),
    DiskStateConfig (..),
    MVR (..),
    MultiVersionConfiguration (..),
    MultiVersionRunner (..),
    makeMultiVersionRunner,
 )
import qualified Concordium.Queries as Q
import Concordium.Skov (
    UpdateResult (..),
 )
import qualified Concordium.Types.Queries as Q

import Concordium.Types.Block
import qualified Data.ProtoLens as Proto
import Lens.Micro.Platform
import qualified Proto.Concordium.Types as Proto
import qualified Proto.Concordium.Types_Fields as ProtoFields

data SenderChannel

type ChannelSendCallback = Ptr SenderChannel -> Ptr Word8 -> Int64 -> IO Int32

-- |Boilerplate wrappers to invoke C callback.
foreign import ccall "dynamic" callChannelSendCallback :: FunPtr ChannelSendCallback -> ChannelSendCallback

mkBlockHash :: BlockHash -> Proto.BlockHash
mkBlockHash bh = Proto.defMessage & ProtoFields.value .~ S.encode bh

mkHeight :: AbsoluteBlockHeight -> Proto.BlockHeight
mkHeight abh = Proto.defMessage & ProtoFields.value .~ fromIntegral abh

mkBakerId :: BakerId -> Proto.BakerId
mkBakerId abh = Proto.defMessage & ProtoFields.bakerId .~ fromIntegral abh

-- A POC that "streams finalized blocks". Each 0.1s it sends the current last finalized block until the channel is closed.
streamFinalized :: StablePtr Ext.ConsensusRunner -> Ptr SenderChannel -> FunPtr (Ptr SenderChannel -> Ptr Word8 -> Int64 -> IO Int32) -> IO Int64
streamFinalized cptr channel cbk = do
    Ext.ConsensusRunner mvr <- deRefStablePtr cptr
    let sender = callChannelSendCallback cbk
    tid <- forkIO $ do
        let go = do
                (cs, bi) <- flip runMVR mvr $ do
                  cs <- Q.getConsensusStatus
                  bi <- Q.getBlockInfo (Q.csLastFinalizedBlock cs)
                  return (cs, bi)
                let msg :: Proto.FinalizedBlockInfo
                    msg =
                        Proto.defMessage
                            & (ProtoFields.hash .~ mkBlockHash (Q.csLastFinalizedBlock cs))
                                . (ProtoFields.height .~ mkHeight (Q.csLastFinalizedBlockHeight cs))
                                . (ProtoFields.maybe'bakerId .~ (mkBakerId <$> (Q.biBlockBaker =<< bi)))
                    encoded = Proto.encodeMessage msg
                r <- BS.unsafeUseAsCStringLen encoded (\(ptr, len) -> sender channel (castPtr ptr) (fromIntegral len))
                when (r == 0 || r == -1) $ do
                    threadDelay 100000
                    go
        go

    return (Ext.toReceiveResult ResultSuccess)

-- * Foreign exports

foreign export ccall streamFinalized :: StablePtr Ext.ConsensusRunner -> Ptr SenderChannel -> FunPtr ChannelSendCallback -> IO Int64
