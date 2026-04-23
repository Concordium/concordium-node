{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module GlobalStateTests.ProtocolLevelTokens where

import Control.Monad.Trans.Class
import Test.HUnit
import Test.Hspec
import Test.QuickCheck as QuickCheck

import qualified Concordium.Crypto.SHA256 as SHA256
import Concordium.Types
import Concordium.Types.Conditionally
import Concordium.Types.HashableTo
import Concordium.Types.Tokens

import Concordium.Crypto.SHA256
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.BlockState.ProtocolLevelTokens
import Concordium.GlobalState.Persistent.BlockState.ProtocolLevelTokens.RustPLTBlockState
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16.Lazy as B16
import Data.Either
import qualified Data.FixedByteString as FBS
import Data.Maybe

-- | Generate a 'TokenRawAmount' value.
genTokenRawAmount :: Gen TokenRawAmount
genTokenRawAmount = TokenRawAmount <$> arbitrary

-- | Run an action in the 'MemBlobStoreT' monad transformer from an empty store.
runWithNewMemBlobStore :: MemBlobStoreT IO a -> IO a
runWithNewMemBlobStore a = do
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
          _pltDecimals = 6
        }

configDEF :: PLTConfiguration
configDEF =
    PLTConfiguration
        { _pltTokenId = tokenDEF,
          _pltModule = TokenModuleRef (SHA256.hash "def"),
          _pltDecimals = 0
        }

emptyPLTPV :: (MonadBlobStore m) => m (ProtocolLevelTokensForPV 'P9)
emptyPLTPV = emptyProtocolLevelTokensFor

testCreateToken :: Assertion
testCreateToken = runWithNewMemBlobStore $ do
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
testSetTokenCirculatingSupply = runWithNewMemBlobStore $ do
    (idxABC, tokens0) <- createToken configABC =<< emptyPLTPV
    (idxDEF, tokens1) <- createToken configDEF tokens0
    tokens2 <- setTokenCirculatingSupply idxABC 100 tokens1
    lift . assertEqual "getTokenCirculatingSupply for ABC returns expected amount" 100
        =<< getTokenCirculatingSupply idxABC tokens2
    lift . assertEqual "getTokenCirculatingSupply for DEF returns expected amount" 0
        =<< getTokenCirculatingSupply idxDEF tokens2
    (hash2 :: ProtocolLevelTokensHash) <- uncond <$> getHashM tokens2
    tokens3 <- setTokenCirculatingSupply idxDEF 200 tokens2
    lift . assertEqual "getTokenCirculatingSupply for ABC returns expected amount" 100
        =<< getTokenCirculatingSupply idxABC tokens3
    lift . assertEqual "getTokenCirculatingSupply for DEF returns expected amount" 200
        =<< getTokenCirculatingSupply idxDEF tokens3
    (hash3 :: ProtocolLevelTokensHash) <- uncond <$> getHashM tokens3
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
    hash6 <- uncond <$> getHashM tokens6
    lift $ assertEqual "Hash of tokens3 and tokens6 should be the same" hash3 hash6

testUpdateTokenState :: Assertion
testUpdateTokenState = runWithNewMemBlobStore $ do
    (idxABC, tokens0) <- createToken configABC =<< emptyPLTPV
    (idxDEF, tokens1) <- createToken configDEF tokens0
    mutableStateABC <- getMutableTokenState idxABC tokens1
    mutableStateDEF <- getMutableTokenState idxDEF tokens1
    (hash1 :: ProtocolLevelTokensHash) <- uncond <$> getHashM tokens1

    lift . assertEqual "lookupTokenState for ABC \"TestKey1\"" Nothing
        =<< lookupTokenState "TestKey1" mutableStateABC
    lift . assertEqual "updateTokenState for ABC \"TestKey1\"" (Just False)
        =<< updateTokenState "TestKey1" (Just "0") mutableStateABC
    lift . assertEqual "lookupTokenState for ABC \"TestKey1\"" (Just "0")
        =<< lookupTokenState "TestKey1" mutableStateABC
    lift . assertEqual "lookupTokenState for DEF \"TestKey1\"" Nothing
        =<< lookupTokenState "TestKey1" mutableStateDEF

    lift . assertEqual "updateTokenState for DEF \"TestKey1\"" (Just False)
        =<< updateTokenState "TestKey1" (Just "1") mutableStateDEF
    lift . assertEqual "lookupTokenState for ABC \"TestKey1\"" (Just "0")
        =<< lookupTokenState "TestKey1" mutableStateABC
    lift . assertEqual "lookupTokenState for DEF \"TestKey1\"" (Just "1")
        =<< lookupTokenState "TestKey1" mutableStateDEF

    lift . assertEqual "updateTokenState for ABC \"TestKey1\"" (Just True)
        =<< updateTokenState "TestKey1" (Just "2") mutableStateABC
    lift . assertEqual "lookupTokenState for ABC \"TestKey1\"" (Just "2")
        =<< lookupTokenState "TestKey1" mutableStateABC
    lift . assertEqual "lookupTokenState for DEF \"TestKey1\"" (Just "1")
        =<< lookupTokenState "TestKey1" mutableStateDEF

    tokens2 <-
        setTokenState idxABC mutableStateABC tokens1
            >>= setTokenState idxDEF mutableStateDEF
    (hash2 :: ProtocolLevelTokensHash) <- uncond <$> getHashM tokens2
    lift $ assertBool "Hash of tokens1 and tokens2 should not be the same" (hash1 /= hash2)
    hash1' <- uncond <$> getHashM tokens1
    lift $ assertEqual "Hash of tokens1 should stay the same" hash1 hash1'

    mutableStateABC2 <- getMutableTokenState idxABC tokens2
    mutableStateDEF2 <- getMutableTokenState idxDEF tokens2
    lift . assertEqual "updateTokenState for DEF \"TestKey2\"" (Just False)
        =<< updateTokenState "TestKey2" (Just "3") mutableStateDEF2
    lift . assertEqual "lookupTokenState for ABC \"TestKey1\"" (Just "2")
        =<< lookupTokenState "TestKey1" mutableStateABC2
    lift . assertEqual "lookupTokenState for DEF \"TestKey1\"" (Just "1")
        =<< lookupTokenState "TestKey1" mutableStateDEF2
    lift . assertEqual "lookupTokenState for DEF \"TestKey2\"" (Just "3")
        =<< lookupTokenState "TestKey2" mutableStateDEF2

    lift . assertEqual "updateTokenState for DEF \"TestKey2\"" (Just True)
        =<< updateTokenState "TestKey2" Nothing mutableStateDEF2
    lift . assertEqual "lookupTokenState for ABC \"TestKey1\"" (Just "2")
        =<< lookupTokenState "TestKey1" mutableStateABC2
    lift . assertEqual "lookupTokenState for DEF \"TestKey1\"" (Just "1")
        =<< lookupTokenState "TestKey1" mutableStateDEF2
    lift . assertEqual "lookupTokenState for DEF \"TestKey2\"" Nothing
        =<< lookupTokenState "TestKey2" mutableStateDEF2

    tokens3 <-
        setTokenState idxABC mutableStateABC2 tokens2
            >>= setTokenState idxDEF mutableStateDEF2
    hash3 <- uncond <$> getHashM tokens3
    lift $ assertEqual "Hash of tokens2 and tokens3 should be the same" hash2 hash3

-- | Asserts "shapshot" of hash of empty 'ProtocolLevelTokens' to make sure it does not change.
snapshotTestHashEmpty :: Assertion
snapshotTestHashEmpty = runWithNewMemBlobStore $ do
    tokensState <- emptyPLTPV

    -- Assert hash
    (hash1 :: ProtocolLevelTokensHash) <- uncond <$> getHashM tokensState
    liftIO $ show hash1 `shouldBe` "c423f9e91ee218b2b5303485dd87a3093a653ddb9bdb839d30aa1924de1dbf05"

-- | Asserts "shapshot" of hash of 'ProtocolLevelTokens' with some simple PLTs to make sure it does not change.
snapshotTestHashSimple :: Assertion
snapshotTestHashSimple = runWithNewMemBlobStore $ do
    tokensState1 <- emptyPLTPV

    -- Create tokens
    let config1 =
            PLTConfiguration
                { _pltTokenId = TokenId "token1",
                  _pltModule = TokenModuleRef $ Hash $ FBS.pack (replicate 32 5),
                  _pltDecimals = 2
                }
    (tokenIndex1, tokensState2) <- createToken config1 tokensState1
    tokensState3 <- setTokenCirculatingSupply tokenIndex1 100 tokensState2
    mutableKeyValueState1 <- getMutableTokenState tokenIndex1 tokensState3
    _ <- updateTokenState (BS.pack [0, 1]) (Just (BS.pack [0, 0])) mutableKeyValueState1
    _ <- updateTokenState (BS.pack [0, 2]) (Just (BS.pack [1, 1])) mutableKeyValueState1
    tokensState4 <- setTokenState tokenIndex1 mutableKeyValueState1 tokensState3
    let config2 =
            PLTConfiguration
                { _pltTokenId = TokenId "token2",
                  _pltModule = TokenModuleRef $ Hash $ FBS.pack (replicate 32 5),
                  _pltDecimals = 4
                }
    (_tokenIndex2, tokensState5) <- createToken config2 tokensState4

    -- Assert hash
    (hash1 :: ProtocolLevelTokensHash) <- uncond <$> getHashM tokensState5
    liftIO $ show hash1 `shouldBe` "d202e9153fea3fdd22c594be21d471c07e9619abc0baad3faca5c81f0bb1504b"

-- | Load empty PLTs state from storage fixture, to make sure we stay compatible.
fixtureTestLoadEmpty :: Assertion
fixtureTestLoadEmpty = runWithNewMemBlobStore $ do
    mbs1 <- liftIO $ newMemBlobStoreWithBytes $ fromRight undefined $ B16.decode "00000000000000080000000000000000"
    flip
        runMemBlobStoreT
        mbs1
        ( do
            -- Load empty PLTs state
            (emptyStateV0 :: ProtocolLevelTokens) <-
                loadDirect $ BlobRef 0
            emptyState <- ProtocolLevelTokensV0 @'P9 <$> refMake emptyStateV0

            -- Assert empty
            pltList <- getPLTList emptyState
            liftIO $ assertEqual "PLTList" (length pltList) 0
        )

-- | Load PLTs state with some simple PLTs from storage fixture, to make sure we stay compatible.
fixtureTestLoadSimple :: Assertion
fixtureTestLoadSimple = runWithNewMemBlobStore $ do
    mbs1 <- liftIO $ newMemBlobStoreWithBytes $ fromRight undefined $ B16.decode "000000000000002806746f6b656e310505050505050505050505050505050505050505050505050505050505050505020000000000000025edbda48b85971b3a874334ca94f07e55e6a6e63eabca968d1257a3223e1b84e14002010100000000000000002503b0eab929105fd6df1ec793cbaf1b554a7a385520a9f7c902adf0219ace6dab4002000000000000000000003648b07111a93452374c7bcf66ee01959af6b4a52cb7cd299341e9ea77b378b0230300000201000000000000005d020000000000000030000000000000000901000000000000008a0000000000000011000000000000000000000000000000c86400000000000000090000000000000000d9000000000000002806746f6b656e3205050505050505050505050505050505050505050505050505050505050505050400000000000000010000000000000000110000000000000103000000000000013300000000000000000900000000000000013c0000000000000021000000000000000201000000000000000000000000000000f20000000000000155"
    flip
        runMemBlobStoreT
        mbs1
        ( do
            -- Load simple PLTs state
            (simpleStateV0 :: ProtocolLevelTokens) <-
                loadDirect $ BlobRef 358
            simpleState <- ProtocolLevelTokensV0 @'P9 <$> refMake simpleStateV0

            -- Assert token state
            pltList <- getPLTList simpleState
            liftIO $ assertEqual "PLTList" (length pltList) 2
            let expectedConfig1 =
                    PLTConfiguration
                        { _pltTokenId = TokenId "token1",
                          _pltModule = TokenModuleRef $ Hash $ FBS.pack (replicate 32 5),
                          _pltDecimals = 2
                        }
            tokenIndex1 <- fromJust <$> getTokenIndex (TokenId "token1") simpleState
            config1 <- getTokenConfiguration tokenIndex1 simpleState
            liftIO $ assertEqual "config1" config1 expectedConfig1
            keyValueState1 <- getMutableTokenState tokenIndex1 simpleState
            value1 <- lookupTokenState (BS.pack [0, 1]) keyValueState1
            liftIO $ assertEqual "value1" value1 (Just $ BS.pack [0, 0])
            value2 <- lookupTokenState (BS.pack [0, 2]) keyValueState1
            liftIO $ assertEqual "value1" value2 (Just $ BS.pack [1, 1])
            let expectedConfig2 =
                    PLTConfiguration
                        { _pltTokenId = TokenId "token2",
                          _pltModule = TokenModuleRef $ Hash $ FBS.pack (replicate 32 5),
                          _pltDecimals = 4
                        }
            tokenIndex2 <- fromJust <$> getTokenIndex (TokenId "token2") simpleState
            config2 <- getTokenConfiguration tokenIndex2 simpleState
            liftIO $ assertEqual "config2" config2 expectedConfig2
        )

tests :: Spec
tests = describe "GlobalStateTests.ProtocolLevelTokens" $ do
    it "createToken" testCreateToken
    it "setTokenCirculatingSupply" testSetTokenCirculatingSupply
    it "updateTokenState" testUpdateTokenState
    it "snapshotHashEmpty" snapshotTestHashEmpty
    it "snapshotHashSimple" snapshotTestHashSimple
    it "fixtureLoadEmpty" fixtureTestLoadEmpty
    it "fixtureLoadSimple" fixtureTestLoadSimple
