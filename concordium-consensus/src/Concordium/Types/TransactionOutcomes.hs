{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.Types.TransactionOutcomes where

import Data.Hashable
import qualified Data.Sequence as Seq
import qualified Data.Serialize as S
import qualified Data.Vector as Vec
import Lens.Micro.Internal
import Lens.Micro.Platform

import qualified Concordium.Crypto.SHA256 as H
import Concordium.Types
import Concordium.Types.Execution
import Concordium.Types.HashableTo
import Concordium.Types.Transactions
import Concordium.Utils.Serialization

-- | Outcomes of transactions. The vector of outcomes must have the same size as the
--  number of transactions in the block, and ordered in the same way.
data TransactionOutcomes = TransactionOutcomes
    { outcomeValues :: !(Vec.Vector TransactionSummary),
      _outcomeSpecial :: !(Seq.Seq SpecialTransactionOutcome)
    }

makeLenses ''TransactionOutcomes

instance Show TransactionOutcomes where
    show (TransactionOutcomes v s) = "Normal transactions: " ++ show (Vec.toList v) ++ ", special transactions: " ++ show s

putTransactionOutcomes :: S.Putter TransactionOutcomes
putTransactionOutcomes TransactionOutcomes{..} = do
    putListOf putTransactionSummary (Vec.toList outcomeValues)
    S.put _outcomeSpecial

getTransactionOutcomes :: SProtocolVersion pv -> S.Get TransactionOutcomes
getTransactionOutcomes spv = TransactionOutcomes <$> (Vec.fromList <$> getListOf (getTransactionSummary spv)) <*> S.get

instance HashableTo (TransactionOutcomesHashV 'TOV0) TransactionOutcomes where
    getHash transactionoutcomes =
        TransactionOutcomesHashV . H.hash . S.runPut $
            putTransactionOutcomes transactionoutcomes

-- | A simple wrapper around a 'H.Hash', that represents the hash of transaction outcomes in a
--  block. The type does not indicate how the hash is derived, which varies over versions.
-- (See 'TransactionOutcomesHashV'.)
--
--   * If the 'TransactionOutcomesVersion' is 'TOV0', then the hash is the hash of a serialized
--     'TransactionOutcomes' structure. (The 'BlockHashVersion' is irrelevant in this case.)
--
--   * If the 'TransactionOutcomesVersion' is 'TOV1', then the hash is the hash of the string
--     @("TransactionOutcomesHashV1" <> outcomes <> special)@, where @outcomes@ and @special@
--     are the version 0 LFMBTree hashes of the normal and special transaction outcomes
--     respectively.
--
--   * If the 'TransactionOutcomesVersion' is 'TOV2', then the hash is the hash of the string
--     @(outcomes <> special)@, where @outcomes@ and @special@ are the version 1 LFMBTree hashes
--     of the normal and special transaction outcomes respectively.
newtype TransactionOutcomesHash = TransactionOutcomesHash {tohGet :: H.Hash}
    deriving newtype (Eq, Ord, Show, S.Serialize, Read, Hashable)

-- | A wrapper around a 'H.Hash', representing the hash of transaction outcomes in
--  a block. The type parameter indicates the hashing scheme used to derive the hash.
--
--   * If the 'TransactionOutcomesVersion' is 'TOV0', then the hash is the hash of a serialized
--     'TransactionOutcomes' structure. (The 'BlockHashVersion' is irrelevant in this case.)
--
--   * If the 'TransactionOutcomesVersion' is 'TOV1', then the hash is the hash of the string
--     @("TransactionOutcomesHashV1" <> outcomes <> special)@, where @outcomes@ and @special@
--     are the version 0 LFMBTree hashes of the normal and special transaction outcomes
--     respectively.
--
--   * If the 'TransactionOutcomesVersion' is 'TOV2', then the hash is the hash of the string
--     @(outcomes <> special)@, where @outcomes@ and @special@ are the version 1 LFMBTree hashes
--     of the normal and special transaction outcomes respectively.
newtype TransactionOutcomesHashV (tov :: TransactionOutcomesVersion) = TransactionOutcomesHashV
    { theTransactionOutcomesHashV :: H.Hash
    }
    deriving newtype (Eq, Ord, Show, S.Serialize, Read, Hashable)

-- | Convert a 'TransactionOutcomesHashV' to a 'TransactionOutcomesHash'. This erases the
--  type-level information about the transaction outcomes hashing version used.
toTransactionOutcomesHash :: TransactionOutcomesHashV tov -> TransactionOutcomesHash
toTransactionOutcomesHash = TransactionOutcomesHash . theTransactionOutcomesHashV

emptyTransactionOutcomesV0 :: TransactionOutcomes
emptyTransactionOutcomesV0 = TransactionOutcomes Vec.empty Seq.empty

-- | Hash of the empty V0 transaction outcomes structure. This transaction outcomes
--  structure is used in protocol versions 1-5.
emptyTransactionOutcomesHashV0 :: TransactionOutcomesHashV 'TOV0
emptyTransactionOutcomesHashV0 = getHash emptyTransactionOutcomesV0

-- | Hash of the empty V1 transaction outcomes structure. This transaction outcomes
--  structure is used in protocol versions 5 and 6.
emptyTransactionOutcomesHashV1 :: TransactionOutcomesHashV 'TOV1
{-# NOINLINE emptyTransactionOutcomesHashV1 #-}
emptyTransactionOutcomesHashV1 =
    TransactionOutcomesHashV $
        H.hashShort
            ( "TransactionOutcomesHashV1"
                <> H.hashToShortByteString (H.hash "EmptyLFMBTree")
                <> H.hashToShortByteString (H.hash "EmptyLFMBTree")
            )

-- | Hash of the empty V1 transaction outcomes structure. This transaction outcomes
--  structure is used starting in protocol version 7.
emptyTransactionOutcomesHashV2 :: TransactionOutcomesHashV 'TOV2
{-# NOINLINE emptyTransactionOutcomesHashV2 #-}
emptyTransactionOutcomesHashV2 =
    TransactionOutcomesHashV $ H.hashOfHashes emptyHash emptyHash
  where
    emptyHash = H.hash $ S.runPut $ do
        S.putWord64be 0
        S.put (H.hash "EmptyLFMBTree")

emptyTransactionOutcomesHashV :: STransactionOutcomesVersion tov -> TransactionOutcomesHashV tov
emptyTransactionOutcomesHashV stov = case stov of
    STOV0 -> emptyTransactionOutcomesHashV0
    STOV1 -> emptyTransactionOutcomesHashV1
    STOV2 -> emptyTransactionOutcomesHashV2

transactionOutcomesV0FromList :: [TransactionSummary] -> TransactionOutcomes
transactionOutcomesV0FromList l =
    let outcomeValues = Vec.fromList l
        _outcomeSpecial = Seq.empty
    in  TransactionOutcomes{..}

type instance Index TransactionOutcomes = TransactionIndex
type instance IxValue TransactionOutcomes = TransactionSummary

instance Ixed TransactionOutcomes where
    ix idx f outcomes@TransactionOutcomes{..} =
        let x = fromIntegral idx
        in  if x >= length outcomeValues
                then pure outcomes
                else ix x f outcomeValues <&> (\ov -> TransactionOutcomes{outcomeValues = ov, ..})
