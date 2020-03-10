{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, FlexibleContexts, DefaultSignatures, UndecidableInstances, DerivingVia, StandaloneDeriving #-}
module Concordium.GlobalState.BlockMonads where

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Concordium.GlobalState.Block as B
import Concordium.GlobalState.BlockPointer as B
import Concordium.Types.Transactions
import Data.Serialize
import Concordium.Types
import qualified Concordium.Crypto.BlockSignature as Sig
import Concordium.GlobalState.Classes as C
import Concordium.GlobalState.AccountTransactionIndex

-- | The goal for this class definition is to abstract the process of converting
-- values that usually exist in two different variants (memory and disk).
--
-- Types are instance of this class if in the scope of the monad @m@ we can
-- convert between the types @a@ and @b@. This type should be trivially implemented
-- by an in-memory representation just embedding the values into the monad.
--
-- This serves two purposes: first associate both types and also provide the functions
-- for moving from one type to another.
--
-- We will consider that the type variable @a@ represents the memory version of the value.
--
-- *NOTE*: although `fromMemoryRepr` is monadic, we must enforce that this function is just
-- a wrapping of a pure function, effectively not making any side effects. Side effects
-- needed for mantaining the invariant should be done somewhere else. This means in
-- practice that for the case of the `PersistentTransaction` we won't write the transaction
-- to the disk inside this function.
class (Monad m) => Convert a b m where
  toMemoryRepr :: b -> m a
  fromMemoryRepr :: a -> m b

instance (Monad (t m), MonadTrans t, Convert a b m) => Convert a b (MGSTrans t m) where
  {-# INLINE toMemoryRepr #-}
  toMemoryRepr = lift . toMemoryRepr
  {-# INLINE fromMemoryRepr #-}
  fromMemoryRepr = lift . fromMemoryRepr

deriving via (MGSTrans MaybeT m) instance Convert a b m => Convert a b (MaybeT m)
deriving via (MGSTrans (ExceptT e) m) instance Convert a b m => Convert a b (ExceptT e m)

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

-- instance (Monad (t m),
--           MonadTrans t,
--           BlockDataMonad b m) => BlockDataMonad b (MGSTrans t m) where
--   {-# INLINE verifyBlockSignature #-}
--   verifyBlockSignature = verifyBlockSignature
--   {-# INLINE putBlock #-}
--   putBlock = lift . putBlock

-- deriving via (MGSTrans MaybeT m) instance BlockDataMonad b m => BlockDataMonad b (MaybeT m)
-- deriving via (MGSTrans (ExceptT e) m) instance BlockDataMonad b m => BlockDataMonad b (ExceptT e m)

-- | Serialize the block by first converting all the transactions to memory transactions
fullBody :: (Serialize t,
            Convert Transaction t m) =>
           BakedBlock t -> m Put
fullBody b = do
  txs :: [Transaction] <- mapM toMemoryRepr (blockTransactions b)
  return $ do
    put (blockSlot b)
    put (blockPointer b)
    put (blockBaker b)
    put (blockProof b)
    put (blockNonce b)
    put (blockLastFinalized b)
    putListOf put txs

instance (Serialize t, Monad m, Convert Transaction t m) =>
         BlockDataMonad (BakedBlock t) m where
  verifyBlockSignature key b = do
    putter <- fullBody b
    return $ Sig.verify key (runPut putter) (bbSignature b)
  putBlock b = do
    putter <- fullBody b
    return $ putter >> put (bbSignature b)

instance (Serialize t, Monad m, Convert Transaction t m) =>
         BlockDataMonad (Block t) m where
  verifyBlockSignature _ GenesisBlock{} = return True
  verifyBlockSignature key (NormalBlock b) = do
    putter <- fullBody b
    return $ Sig.verify key (runPut putter) (bbSignature b)
  putBlock (GenesisBlock gd) = return $ put genesisSlot >> put gd
  putBlock (NormalBlock b) = do
    putter <- fullBody b
    return $ putter >> put (bbSignature b)

instance (Serialize t, Monad m, Convert Transaction t m) =>
         BlockDataMonad (B.PendingBlock t) m where
  verifyBlockSignature key = verifyBlockSignature key . pbBlock
  putBlock = putBlock . pbBlock

instance (Serialize t, Monad m, Convert Transaction t m) =>
         BlockDataMonad (B.BlockPointer ati t p s) m where
  verifyBlockSignature key = verifyBlockSignature key . _bpBlock
  putBlock = putBlock . _bpBlock

-- | This typeclass abstracts the access to the parent and last finalized blocks
-- using the pointers inside the `BlockPointer t p s`.
class (Monad m, GlobalStateTypes m, ATITypes m) => BlockPointerMonad m where
    -- |Get the 'BlockState' of a 'BlockPointer'.
    blockState :: C.BlockPointer m -> m (BlockState m)

    -- |Get the parent block of a 'BlockPointer'
    bpParent :: C.BlockPointer m -> m (C.BlockPointer m)

    -- |Get the last finalized block of a 'BlockPointer'
    bpLastFinalized :: C.BlockPointer m -> m (C.BlockPointer m)

    -- |Get the block transaction affect.
    bpTransactionAffectSummaries :: C.BlockPointer m -> m (ATIStorage m)
    default bpTransactionAffectSummaries :: ATIStorage m ~ () => C.BlockPointer m -> m (ATIStorage m)
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
signBlock :: (Convert Transaction t m, Serialize t) =>
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
