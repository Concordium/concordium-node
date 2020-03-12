{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, FlexibleContexts, DefaultSignatures, UndecidableInstances, DerivingVia, StandaloneDeriving #-}
module Concordium.GlobalState.BlockMonads where

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Concordium.GlobalState.Block as B
import Concordium.GlobalState.BlockPointer as B
import Concordium.GlobalState.Types
import Concordium.Types.Transactions
import Data.Serialize
import Concordium.Types
import qualified Concordium.Crypto.BlockSignature as Sig
import Concordium.GlobalState.Classes as C
import Concordium.GlobalState.AccountTransactionIndex

-- |The 'BlockDataMonad' class provides an interface for the maybe monadic data associated
-- with a block.
--
-- Depending on the implementation, this functions might require to use the monadic
-- context to access the data (for example if we are trying to verify a block that has
-- persistent transactions inside we will need to first read those from the disk).
--
-- In the context of a monad that is capable of converting its transaction type into
-- memory transactions, this class is implemented trivially using `fullBody`. In such
-- monadic context, we automatically get instances for:
--
-- * BlockDataMonad (BakedBlock t)
-- * BlockDataMonad (Block t)
-- * BlockDataMonad (PendingBlock t)
-- * BlockDataMonad (BlockPointer t)
class (Monad m,
       BlockData b,
       Convert Transaction (BlockTransactionType b) m) => BlockDataMonad b m where
  -- |Determine if the block is signed by the given key
  -- (always 'True' for genesis block)
  verifyBlockSignature :: Sig.VerifyKey -> b -> m Bool
  -- |Serialize the block with all its contents.
  putBlock :: b -> m Put

instance (ToPut t, Monad m, Convert Transaction t m) =>
         BlockDataMonad (BakedBlock t) m where
  verifyBlockSignature key b = do
    putter <- fullBody b
    return $ Sig.verify key (runPut putter) (bbSignature b)
  putBlock b = do
    putter <- fullBody b
    return $ putter >> put (bbSignature b)

instance (ToPut t, Monad m, Convert Transaction t m) =>
         BlockDataMonad (Block t) m where
  verifyBlockSignature _ GenesisBlock{} = return True
  verifyBlockSignature key (NormalBlock b) = do
    putter <- fullBody b
    return $ Sig.verify key (runPut putter) (bbSignature b)
  putBlock (GenesisBlock gd) = return $ put genesisSlot >> put gd
  putBlock (NormalBlock b) = do
    putter <- fullBody b
    return $ putter >> put (bbSignature b)

instance (ToPut t, Monad m, Convert Transaction t m) =>
         BlockDataMonad (B.PendingBlock t) m where
  verifyBlockSignature key = verifyBlockSignature key . pbBlock
  putBlock = putBlock . pbBlock

instance (ToPut t, Monad m, Convert Transaction t m) =>
         BlockDataMonad (B.BlockPointer ati t p s) m where
  verifyBlockSignature key = verifyBlockSignature key . _bpBlock
  putBlock = putBlock . _bpBlock

-- | This typeclass abstracts the access to the parent and last finalized blocks
-- using the pointers inside the `BlockPointer t p s`.
class (Monad m, GlobalStateTypes m, ATITypes m) => BlockPointerMonad m where
    -- |Get the 'BlockState' of a 'BlockPointer'.
    blockState :: BlockPointerType m -> m (BlockState m)

    -- |Get the parent block of a 'BlockPointer'
    bpParent :: BlockPointerType m -> m (BlockPointerType m)

    -- |Get the last finalized block of a 'BlockPointer'
    bpLastFinalized :: BlockPointerType m -> m (BlockPointerType m)

    -- |Get the block transaction affect.
    bpTransactionAffectSummaries :: BlockPointerType m -> m (ATIStorage m)
    default bpTransactionAffectSummaries :: ATIStorage m ~ () => BlockPointerType m -> m (ATIStorage m)
    bpTransactionAffectSummaries = \_ -> return ()
    {-# INLINE bpTransactionAffectSummaries #-}

instance (Monad (t m), MonadTrans t, BlockPointerMonad m) => BlockPointerMonad (MGSTrans t m) where
  {-# INLINE blockState #-}
  blockState = lift . blockState
  {-# INLINE bpParent #-}
  bpParent = lift . bpParent
  {-# INLINE bpLastFinalized #-}
  bpLastFinalized = lift . bpLastFinalized
  {-# INLINE bpTransactionAffectSummaries #-}
  bpTransactionAffectSummaries = lift . bpTransactionAffectSummaries


deriving via (MGSTrans MaybeT m) instance BlockPointerMonad m => BlockPointerMonad (MaybeT m)
deriving via (MGSTrans (ExceptT e) m) instance BlockPointerMonad m => BlockPointerMonad (ExceptT e m)

-- |Generate a baked block.
signBlock :: forall t m. (Convert (BakedBlock Transaction) (BakedBlock t) m) =>
   BakerSignPrivateKey           -- ^Key for signing the new block
    -> Slot                       -- ^Block slot (must be non-zero)
    -> BlockHash                  -- ^Hash of parent block
    -> BakerId                    -- ^Identifier of block baker
    -> BlockProof                 -- ^Block proof
    -> BlockNonce                 -- ^Block nonce
    -> BlockHash                  -- ^Hash of last finalized block
    -> [t]                        -- ^List of transactions
    -> m (BakedBlock t)
signBlock key slot parent baker proof bnonce lastFin transactions
    | slot == 0 = error "Only the genesis block may have slot 0"
    | otherwise = do
        putter <- fullBody (preBlock undefined)
        let sig = Sig.sign key . runPut $ putter
            block = preBlock sig
        return block
    where
        preBlock = BakedBlock slot (BlockFields parent baker proof bnonce lastFin) transactions
