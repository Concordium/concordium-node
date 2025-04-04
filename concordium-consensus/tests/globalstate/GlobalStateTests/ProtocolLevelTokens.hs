{-# LANGUAGE TypeApplications #-}

module GlobalStateTests.ProtocolLevelTokens where

import Control.Monad
import qualified Data.ByteString as BS
import Data.Serialize
import Test.Hspec
import Test.QuickCheck as QuickCheck

import Concordium.GlobalState.Persistent.BlockState.ProtocolLevelTokens

genTokenRawAmount :: Gen TokenRawAmount
genTokenRawAmount = TokenRawAmount <$> arbitrary

-- | Deserialize a value, ensuring that the input it fully consumed.
decodeFull :: (Serialize a) => BS.ByteString -> Either String a
decodeFull =
    runGet
        ( do
            g <- get
            done <- isEmpty
            unless done $ fail "Input was not fully consumed"
            return g
        )

testEncodeDecode :: Property
testEncodeDecode = forAll genTokenRawAmount $ \a ->
    let encoded = encode a
    in  QuickCheck.label ("encoded length " ++ show (BS.length encoded)) $
            decodeFull encoded === Right a

testDecodeFailures :: Spec
testDecodeFailures =
    describe "Failing TokenRawAmount deserialization cases" $ mapM_ testFail examples
  where
    testFail (bytes, expct) =
        it ("Decoding " ++ show bytes) $ decodeFull @TokenRawAmount (BS.pack bytes) `shouldBe` Left expct
    examples =
        [ ([0x80], noPadding),
          ([0x80, 0x00], noPadding),
          ([0x81], unexpectedEnd),
          ([0x82, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x00], outOfRange),
          ([0x82, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80], outOfRange),
          ([0x81, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x00], outOfRange)
        ]
    noPadding = "Failed reading: Padding bytes are not allowed\nEmpty call stack\n"
    unexpectedEnd = "too few bytes\nFrom:\tdemandInput\n\n"
    outOfRange = "Failed reading: Value out of range\nEmpty call stack\n"

tests :: Spec
tests = describe "GlobalStateTests.ProtocolLevelTokens" $ do
    it "Encode and decode TokenRawAmount" $ withMaxSuccess 10000 testEncodeDecode
    testDecodeFailures
