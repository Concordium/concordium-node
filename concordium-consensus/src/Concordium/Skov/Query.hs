{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, DerivingStrategies, DerivingVia, UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
module Concordium.Skov.Query where

import Control.Monad
import Data.Functor
import Control.Monad.Trans.State hiding (gets)
import Control.Monad.State.Class
import qualified Data.Sequence as Seq

import Concordium.GlobalState.TreeState
import qualified Concordium.GlobalState.TreeState.Basic as Basic

import Concordium.Skov.Monad
import Concordium.Logger
import Concordium.TimeMonad

-- |This wrapper endows a monad that implements 'TreeStateMonad' with
-- an instance of 'SkovQueryMonad'.
newtype TSSkovWrapper m a = TSSkovWrapper {runTSSkovWrapper :: m a}
    deriving (Functor, Applicative, Monad, BlockStateOperations, BlockStateQuery, TreeStateMonad, TimeMonad, LoggerMonad)
type instance BlockPointer (TSSkovWrapper m) = BlockPointer m
type instance UpdatableBlockState (TSSkovWrapper m) = UpdatableBlockState m

instance (TreeStateMonad m) => SkovQueryMonad (TSSkovWrapper m) where
    {-# INLINE resolveBlock #-}
    resolveBlock cbp = getBlockStatus cbp <&> \case
            Just (BlockAlive bp) -> Just bp
            Just (BlockFinalized bp _) -> Just bp
            _ -> Nothing
    {-# INLINE isFinalized #-}
    isFinalized = getBlockStatus >=> \case
            Just (BlockFinalized _ _) -> return True
            _ -> return False
    lastFinalizedBlock = getLastFinalized
    getGenesisData = Concordium.GlobalState.TreeState.getGenesisData
    genesisBlock = getGenesisBlockPointer
    getCurrentHeight = do
            lfHeight <- getLastFinalizedHeight
            branchLen <- fromIntegral . Seq.length <$> getBranches
            return $ lfHeight + branchLen
    branchesFromTop = revSeqToList <$> getBranches
        where
            revSeqToList Seq.Empty = []
            revSeqToList (r Seq.:|> t) = t : revSeqToList r
    getBlocksAtHeight h = do
            lastFin <- getLastFinalized
            case compare h (bpHeight lastFin) of
                EQ -> return [lastFin]
                GT -> do
                    brs <- getBranches
                    case brs Seq.!? (fromIntegral $ h - bpHeight lastFin - 1) of
                        Nothing -> return []
                        Just bs -> return bs
                LT -> return [findFrom (bpParent lastFin)] -- TODO: replace with more efficient search
        where
            findFrom bp
                | bpHeight bp == h = bp
                | otherwise = findFrom (bpParent bp)

-- UndecidableInstances is required to allow these type instance declarations.
type instance BlockPointer (SimpleSkovMonad s m) = BlockPointer (Basic.SkovTreeState s (StateT s m))
type instance UpdatableBlockState (SimpleSkovMonad s m) = UpdatableBlockState (Basic.SkovTreeState s (StateT s m))

-- |The 'SimpleSkovMonad' wraps 'StateT' to provide an instance of 'SkovQueryMonad'
-- when the state implements 'SkovLenses'.
newtype SimpleSkovMonad s m a = SimpleSkovMonad {runSimpleSkovMonad :: StateT s m a}
    deriving (Functor, Applicative, Monad, TimeMonad, LoggerMonad, MonadState s)
    deriving BlockStateQuery via (Basic.SkovTreeState s (StateT s m))
    deriving BlockStateOperations via (Basic.SkovTreeState s (StateT s m))
    deriving TreeStateMonad via (Basic.SkovTreeState s (StateT s m))
    deriving SkovQueryMonad via (TSSkovWrapper (Basic.SkovTreeState s (StateT s m)))

-- |Evaluate an action in the 'SimpleSkovMonad'.  This is intended for
-- running queries against the state (i.e. with no updating side-effects).
evalSSM :: (Monad m) => SimpleSkovMonad s m a -> s -> m a
evalSSM (SimpleSkovMonad a) st = evalStateT a st
