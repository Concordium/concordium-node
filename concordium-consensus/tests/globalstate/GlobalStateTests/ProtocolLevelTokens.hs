{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module GlobalStateTests.ProtocolLevelTokens where

import Control.Monad
import Control.Monad.Trans.Class
import qualified Data.ByteString as BS
import Data.Serialize
import Test.HUnit
import Test.Hspec
import Test.QuickCheck as QuickCheck

import qualified Concordium.Crypto.SHA256 as SHA256
import Concordium.Types
import Concordium.Types.HashableTo

import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.BlockState.ProtocolLevelTokens

-- | Generate a 'TokenRawAmount' value.
genTokenRawAmount :: Gen TokenRawAmount
genTokenRawAmount = TokenRawAmount <$> arbitrary

-- | Deserialize a value, ensuring that the input is fully consumed.
decodeFull :: (Serialize a) => BS.ByteString -> Either String a
decodeFull =
    runGet
        ( do
            g <- get
            done <- isEmpty
            unless done $ fail "Input was not fully consumed"
            return g
        )

-- | Test that encoding and decoding a 'TokenRawAmount' value works as expected.
testEncodeDecode :: Property
testEncodeDecode = forAll genTokenRawAmount $ \a ->
    let encoded = encode a
    in  QuickCheck.label ("encoded length " ++ show (BS.length encoded)) $
            decodeFull encoded === Right a

-- | Test cases where decoding a 'TokenRawAmount' fails.
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

-- | Run an action in the 'MemBlobStoreT' monad transformer from an empty store.
runBlobStore :: MemBlobStoreT IO a -> IO a
runBlobStore a = do
    mbs <- newMemBlobStore
    runMemBlobStoreT a mbs

tokenABC :: TokenId
tokenABC = TokenId "ABC"

tokenDEF :: TokenId
tokenDEF = TokenId "DEF"

configABC :: PLTConfiguration
configABC =
    PLTConfiguration
        { _pltTokenId = tokenABC,
          _pltModule = TokenModuleRef (SHA256.hash "abc"),
          _pltDecimals = 6,
          _pltGovernanceAccountIndex = 0
        }

configDEF :: PLTConfiguration
configDEF =
    PLTConfiguration
        { _pltTokenId = tokenDEF,
          _pltModule = TokenModuleRef (SHA256.hash "def"),
          _pltDecimals = 0,
          _pltGovernanceAccountIndex = 0
        }

emptyPLTPV :: (MonadBlobStore m) => m (ProtocolLevelTokensForPV 'P9)
emptyPLTPV = emptyProtocolLevelTokensForPV

testCreateToken :: Assertion
testCreateToken = runBlobStore $ do
    (idx, tokens) <- createToken configABC =<< emptyPLTPV
    checks0 idx tokens
    (idx', tokens') <- createToken configDEF tokens
    checks1 idx idx' tokens'
    tokens'' <- loadRef =<< storeRef tokens'
    checks1 idx idx' tokens''
  where
    checks0 idx tokens = do
        lift (assertEqual "ABC Token index" 0 idx)
        lift . assertEqual "getTokenIndex for ABC returns expected index" (Just idx)
            =<< getTokenIndex tokenABC tokens
        lift . assertEqual "getTokenIndex for DEF returns Nothing" Nothing
            =<< getTokenIndex tokenDEF tokens
        lift . assertEqual "getTokenCirculatingSupply for ABC returns expected amount" 0
            =<< getTokenCirculatingSupply idx tokens
        lift . assertEqual "getTokenConfiguration for ABC returns expected configuration" configABC
            =<< getTokenConfiguration idx tokens
    checks1 idx idx' tokens' = do
        lift (assertEqual "DEF Token index" 1 idx')
        lift . assertEqual "getTokenIndex for ABC returns expected index" (Just idx)
            =<< getTokenIndex tokenABC tokens'
        lift . assertEqual "getTokenIndex for DEF returns expected index" (Just idx')
            =<< getTokenIndex tokenDEF tokens'
        lift . assertEqual "getTokenCirculatingSupply for ABC returns expected amount" 0
            =<< getTokenCirculatingSupply idx tokens'
        lift . assertEqual "getTokenConfiguration for ABC returns expected configuration" configABC
            =<< getTokenConfiguration idx tokens'
        lift . assertEqual "getTokenCirculatingSupply for DEF returns expected amount" 0
            =<< getTokenCirculatingSupply idx' tokens'
        lift . assertEqual "getTokenConfiguration for DEF returns expected configuration" configDEF
            =<< getTokenConfiguration idx' tokens'

testSetTokenCirculatingSupply :: Assertion
testSetTokenCirculatingSupply = runBlobStore $ do
    (idxABC, tokens0) <- createToken configABC =<< emptyPLTPV
    (idxDEF, tokens1) <- createToken configDEF tokens0
    tokens2 <- setTokenCirculatingSupply idxABC 100 tokens1
    lift . assertEqual "getTokenCirculatingSupply for ABC returns expected amount" 100
        =<< getTokenCirculatingSupply idxABC tokens2
    lift . assertEqual "getTokenCirculatingSupply for DEF returns expected amount" 0
        =<< getTokenCirculatingSupply idxDEF tokens2
    hash2 <- getHashM @_ @ProtocolLevelTokensHash tokens2
    tokens3 <- setTokenCirculatingSupply idxDEF 200 tokens2
    lift . assertEqual "getTokenCirculatingSupply for ABC returns expected amount" 100
        =<< getTokenCirculatingSupply idxABC tokens3
    lift . assertEqual "getTokenCirculatingSupply for DEF returns expected amount" 200
        =<< getTokenCirculatingSupply idxDEF tokens3
    hash3 <- getHashM @_ @ProtocolLevelTokensHash tokens3
    lift $ assertBool "Hash of tokens2 and tokens3 should be different" (hash2 /= hash3)
    tokens4 <- setTokenCirculatingSupply idxABC 0 tokens3
    lift . assertEqual "getTokenCirculatingSupply for ABC returns expected amount" 0
        =<< getTokenCirculatingSupply idxABC tokens4
    lift . assertEqual "getTokenCirculatingSupply for DEF returns expected amount" 200
        =<< getTokenCirculatingSupply idxDEF tokens4
    tokens5 <- cache =<< loadRef =<< storeRef tokens4
    lift . assertEqual "getTokenCirculatingSupply for ABC returns expected amount" 0
        =<< getTokenCirculatingSupply idxABC tokens5
    lift . assertEqual "getTokenCirculatingSupply for DEF returns expected amount" 200
        =<< getTokenCirculatingSupply idxDEF tokens5
    tokens6 <- setTokenCirculatingSupply idxABC 100 tokens5
    lift . assertEqual "getTokenCirculatingSupply for ABC returns expected amount" 100
        =<< getTokenCirculatingSupply idxABC tokens6
    lift . assertEqual "getTokenCirculatingSupply for DEF returns expected amount" 200
        =<< getTokenCirculatingSupply idxDEF tokens6
    hash6 <- getHashM tokens6
    lift $ assertEqual "Hash of tokens3 and tokens6 should be the same" hash3 hash6

testSetTokenState :: Assertion
testSetTokenState = runBlobStore $ do
    (idxABC, tokens0) <- createToken configABC =<< emptyPLTPV
    (idxDEF, tokens1) <- createToken configDEF tokens0
    lift . assertEqual "getTokenState for ABC \"TestKey1\"" Nothing
        =<< getTokenState idxABC "TestKey1" tokens1
    tokens2 <- setTokenState idxABC "TestKey1" (Just "0") tokens1
    lift . assertEqual "getTokenState for ABC \"TestKey1\"" (Just "0")
        =<< getTokenState idxABC "TestKey1" tokens2
    lift . assertEqual "getTokenState for DEF \"TestKey1\"" Nothing
        =<< getTokenState idxDEF "TestKey1" tokens2
    tokens3 <- setTokenState idxDEF "TestKey1" (Just "1") tokens2
    lift . assertEqual "getTokenState for ABC \"TestKey1\"" (Just "0")
        =<< getTokenState idxABC "TestKey1" tokens3
    lift . assertEqual "getTokenState for DEF \"TestKey1\"" (Just "1")
        =<< getTokenState idxDEF "TestKey1" tokens3
    tokens4 <- setTokenState idxABC "TestKey1" (Just "2") tokens3
    lift . assertEqual "getTokenState for ABC \"TestKey1\"" (Just "2")
        =<< getTokenState idxABC "TestKey1" tokens4
    lift . assertEqual "getTokenState for DEF \"TestKey1\"" (Just "1")
        =<< getTokenState idxDEF "TestKey1" tokens4
    hash4 <- getHashM @_ @ProtocolLevelTokensHash tokens4
    tokens5 <- setTokenState idxDEF "TestKey2" (Just "3") tokens4
    lift . assertEqual "getTokenState for ABC \"TestKey1\"" (Just "2")
        =<< getTokenState idxABC "TestKey1" tokens5
    lift . assertEqual "getTokenState for DEF \"TestKey1\"" (Just "1")
        =<< getTokenState idxDEF "TestKey1" tokens5
    lift . assertEqual "getTokenState for DEF \"TestKey2\"" (Just "3")
        =<< getTokenState idxDEF "TestKey2" tokens5
    tokens6 <- setTokenState idxDEF "TestKey2" Nothing tokens5
    lift . assertEqual "getTokenState for ABC \"TestKey1\"" (Just "2")
        =<< getTokenState idxABC "TestKey1" tokens6
    lift . assertEqual "getTokenState for DEF \"TestKey1\"" (Just "1")
        =<< getTokenState idxDEF "TestKey1" tokens6
    lift . assertEqual "getTokenState for DEF \"TestKey2\"" Nothing
        =<< getTokenState idxDEF "TestKey2" tokens6
    hash6 <- getHashM tokens6
    lift $ assertEqual "Hash of tokens4 and tokens6 should be the same" hash4 hash6

tests :: Spec
tests = describe "GlobalStateTests.ProtocolLevelTokens" $ do
    it "Encode and decode TokenRawAmount" $ withMaxSuccess 10000 testEncodeDecode
    testDecodeFailures
    it "createToken" testCreateToken
    it "setTokenCirculatingSupply" testSetTokenCirculatingSupply
    it "setTokenState" testSetTokenState
