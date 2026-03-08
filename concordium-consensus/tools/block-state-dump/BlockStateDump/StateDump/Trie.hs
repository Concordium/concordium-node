{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module BlockStateDump.StateDump.Trie where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Bits as Bits
import Data.Word

import qualified Concordium.Crypto.SHA256 as Hash
import qualified Concordium.Types.HashableTo as Hash

import qualified Concordium.GlobalState.Persistent.BlobStore as Blob
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import qualified Concordium.GlobalState.Persistent.LFMBTree as LFMB
import qualified Concordium.GlobalState.Persistent.Trie as Trie

import BlockStateDump.Shared
import qualified Concordium.Crypto.ByteStringHelpers as BSH
import qualified Concordium.ID.Types as ID
import Concordium.Types
import Text.Printf

dumpTrie ::
    forall pv m k v.
    ( BS.SupportsPersistentState pv m,
      Blob.BlobStorable m v,
      Hash.MHashableTo m Hash.Hash (Trie.TrieF k v (Blob.UnbufferedFix (Trie.TrieF k v)))
    ) =>
    OutputFiles ->
    String ->
    NodeId ->
    Trie.TrieN Blob.UnbufferedFix k v ->
    (NodeId -> v -> m ()) ->
    m ()
dumpTrie output name rootParentNode tree dumpLeaf = do
    case tree of
        Trie.EmptyTrieN -> do
            _rootNode <- liftIO $ buildCompNode output (name ++ "{size=0}") rootParentNode Nothing
            return ()
        Trie.TrieN size (Blob.UnbufferedFix tRef) -> do
            rootNode <- liftIO $ buildCompNode output (name ++ "{size=" ++ show size ++ "}") rootParentNode Nothing
            t <- Blob.refLoad tRef
            blobRef <- getURRef tRef
            hash <- Hash.getHashM tRef
            dumpTrieT (blobRefNodeBuilder output rootNode "root" blobRef hash) t
  where
    dumpTrieT :: BuildNode -> Trie.TrieF k v (Blob.UnbufferedFix (Trie.TrieF k v)) -> m ()
    dumpTrieT nodeBuilder = \case
        Trie.Branch (Trie.Branches branchesArray) -> do
            maybeNode <- liftIO $ nodeBuilder (name ++ "{branch}")
            forM_ maybeNode $ \node -> do
                forM_ branchesArray $ \(Trie.BranchEntry index (Blob.UnbufferedFix branchRef)) -> do
                    branch <- Blob.refLoad branchRef
                    blobRef <- getURRef branchRef
                    hash <- Hash.getHashM branchRef
                    dumpTrieT (blobRefNodeBuilder output node (printf "%02x" index) blobRef hash) branch
        Trie.Stem stem (Blob.UnbufferedFix tRef) -> do
            maybeNode <- liftIO $ nodeBuilder (name ++ "{stem}")
            forM_ maybeNode $ \node -> do
                let hex = show $ BSH.ShortByteStringHex stem
                t <- Blob.refLoad tRef
                blobRef <- getURRef tRef
                hash <- Hash.getHashM tRef
                dumpTrieT (blobRefNodeBuilder output node hex blobRef hash) t
        Trie.Tip v -> do
            maybeNode <- liftIO $ nodeBuilder (name ++ "{tip}")
            forM_ maybeNode $ \node -> do
                dumpLeaf node v
                return ()
