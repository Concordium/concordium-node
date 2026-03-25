{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module BlockStateDump.StateDump.Trie where

import Control.Monad
import Text.Printf

import qualified Concordium.Crypto.ByteStringHelpers as BSH

import qualified Concordium.GlobalState.Persistent.BlobStore as Blob
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import qualified Concordium.GlobalState.Persistent.Trie as Trie

import BlockStateDump.Shared
import Control.Monad.Trans (lift)

dumpTrie ::
    forall pv m k v.
    ( BS.SupportsPersistentState pv m,
      Blob.BlobStorable m v
    ) =>
    String ->
    NodeId ->
    Trie.TrieN Blob.UnbufferedFix k v ->
    (BuildNode m -> v -> StateDumpMonad m ()) ->
    StateDumpMonad m ()
dumpTrie name rootParentNode tree dumpLeaf = do
    case tree of
        Trie.EmptyTrieN -> do
            _rootNode <- buildCompNode (name ++ "{size=0}") rootParentNode Nothing
            return ()
        Trie.TrieN size (Blob.UnbufferedFix tRef) -> do
            rootNode <- buildCompNode (name ++ "{size=" ++ show size ++ "}") rootParentNode Nothing
            t <- lift $ Blob.refLoad tRef
            blobRef <- lift $ getURRef tRef
            dumpTrieT (blobRefNodeBuilder rootNode "root" blobRef Nothing) t
  where
    dumpTrieT :: BuildNode m -> Trie.TrieF k v (Blob.UnbufferedFix (Trie.TrieF k v)) -> StateDumpMonad m ()
    dumpTrieT nodeBuilder = \case
        Trie.Branch (Trie.Branches branchesArray) -> do
            maybeNode <- nodeBuilder (name ++ "{branch}")
            forM_ maybeNode $ \node -> do
                forM_ branchesArray $ \(Trie.BranchEntry index (Blob.UnbufferedFix branchRef)) -> do
                    branch <- lift $ Blob.refLoad branchRef
                    blobRef <- lift $ getURRef branchRef
                    dumpTrieT (blobRefNodeBuilder node (printf "%02x" index) blobRef Nothing) branch
        Trie.Stem stem (Blob.UnbufferedFix tRef) -> do
            maybeNode <- nodeBuilder (name ++ "{stem}")
            forM_ maybeNode $ \node -> do
                let hex = show $ BSH.ShortByteStringHex stem
                let edgeLabel = (take 6 hex) ++ if (length hex > 6) then ".." else ""
                t <- lift $ Blob.refLoad tRef
                blobRef <- lift $ getURRef tRef
                dumpTrieT (blobRefNodeBuilder node edgeLabel blobRef Nothing) t
        Trie.Tip v -> do
            dumpLeaf nodeBuilder v
