{-# LANGUAGE TypeApplications #-}

-- | This module provides helper functions for marshalling types across the FFI boundary.
module Concordium.External.Helpers where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString.Unsafe as BS
import Data.Coerce
import qualified Data.Serialize as S
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.Word
import Foreign

import Concordium.Crypto.SHA256
import Concordium.ID.Types
import Concordium.Types
import qualified Concordium.Types.Queries as Queries
import qualified Concordium.Wasm as Wasm
import qualified Data.FixedByteString as FBS

-- | Decode a 'Queries.BlockHashInput' from a tag and a pointer to the payload data.
--  The tags (and corresponding payloads) are as follows:
--
--    * 0 (no payload): best block
--    * 1 (no payload): last finalized block
--    * 2 (block hash): specified block by hash
--    * 3 (64-bit big-endian absolute block height): specified block by absolute height
--    * 4 (64-bit BE relative height, 32-bit BE genesis index, bool (1 byte) flag to restrict to
--      specified genesis index): specified block by relative height
decodeBlockHashInput :: Word8 -> Ptr Word8 -> IO Queries.BlockHashInput
decodeBlockHashInput 0 _ = return Queries.Best
decodeBlockHashInput 1 _ = return Queries.LastFinal
decodeBlockHashInput 2 hsh = Queries.Given . coerce <$> FBS.create @DigestSize (\p -> copyBytes p hsh 32)
decodeBlockHashInput n dt =
    Queries.AtHeight
        <$> case n of
            3 -> do
                inputData <- BS.unsafePackCStringLen (castPtr dt, 8) -- 8 bytes for the block height.
                case S.decode inputData of
                    Left err -> error $ "Precondition violation in FFI call: " ++ err
                    Right aBlockHeight -> return $ Queries.Absolute{..}
            4 -> do
                inputData <- BS.unsafePackCStringLen (castPtr dt, 13) -- 8 bytes for the block height, 4 bytes for the genesis index and 1 byte for encoding 'restrict'.
                case S.decode inputData of
                    Left err -> error $ "Precondition violation in FFI call: " ++ err
                    Right (rBlockHeight, rGenesisIndex, rRestrict) -> return $ Queries.Relative{..}
            _ -> error "Precondition violation in FFI call: Unknown block hash input type"

-- | Decode an 'Queries.EpochRequest' given the tag byte and data.
--  The tags supported by 'decodeBlockHashInput' are also supported here (0-4), corresponding to
--  a 'Queries.EpochOfBlock'. The tag 5 is used for 'Queries.SpecifiedEpoch'.
decodeEpochRequest :: Word8 -> Ptr Word8 -> IO Queries.EpochRequest
decodeEpochRequest 5 dt = do
    -- 8 bytes for epoch, 4 bytes for genesis index
    inputData <- BS.unsafePackCStringLen (castPtr dt, 12)
    case S.decode inputData of
        Left err -> error $ "Precondition violation in FFI call: " ++ err
        Right (erEpoch, erGenesisIndex) -> return $! Queries.SpecifiedEpoch{..}
decodeEpochRequest n dt = Queries.EpochOfBlock <$> decodeBlockHashInput n dt

-- | Decode an account address from a foreign ptr. Assumes 32 bytes are available.
decodeAccountAddress :: Ptr Word8 -> IO AccountAddress
decodeAccountAddress accPtr = coerce <$> FBS.create @AccountAddressSize (\p -> copyBytes p accPtr 32)

-- | NB: Assumes the data is at least 32 bytes.
decodeTransactionHashInput :: Ptr Word8 -> IO TransactionHash
decodeTransactionHashInput hsh = coerce <$> FBS.create @DigestSize (\p -> copyBytes p hsh 32)

-- | Decode an account address from a foreign ptr.
decodeAccountIdentifierInput :: Word8 -> Ptr Word8 -> IO AccountIdentifier
decodeAccountIdentifierInput 0 dta = AccAddress <$> decodeAccountAddress dta
decodeAccountIdentifierInput 1 dta = do
    bs <- BS.unsafePackCStringLen (castPtr dta, 48)
    case S.decode bs of
        Left err -> error $ "Precondition violation in FFI call: " ++ err
        Right cid -> return (CredRegID cid)
decodeAccountIdentifierInput 2 dta = AccIndex . AccountIndex <$> peek (castPtr dta)
decodeAccountIdentifierInput n _ = error $ "Unknown account identifier tag: " ++ show n

decodeModuleRefInput :: Ptr Word8 -> IO ModuleRef
decodeModuleRefInput modRef = coerce <$> FBS.create @DigestSize (\p -> copyBytes p modRef 32)

-- | NB: Assumes the data is valid utf8. The caller is expected to guarantee
--  this.
decodeText :: Ptr Word8 -> Word32 -> IO Text
decodeText ptr len = Text.decodeUtf8 <$> BS.packCStringLen (castPtr ptr, fromIntegral len)

-- | NB: Assumes the data is valid utf8. Protobuf guarantees this, and Rust/tonic
--  does actually implement the validation, so this is safe.
decodeReceiveName :: Ptr Word8 -> Word32 -> IO Wasm.ReceiveName
decodeReceiveName ptr len = Wasm.ReceiveName <$> decodeText ptr len

-- | Decode a smart contract parameter from the given pointer and length.
decodeParameter :: Ptr Word8 -> Word32 -> IO Wasm.Parameter
decodeParameter ptr len =
    Wasm.Parameter . BSS.toShort <$> BS.packCStringLen (castPtr ptr, fromIntegral len)

-- | Decode a 'TokenId' from the given pointer and length.
decodeTokenId :: Ptr Word8 -> Word8 -> IO (Either String TokenId)
decodeTokenId ptr len =
    makeTokenId . BSS.toShort <$> BS.packCStringLen (castPtr ptr, fromIntegral len)
