{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module GlobalStateTests.ProtocolLevelTokens where

import Control.Monad.Trans.Class
import Test.HUnit
import Test.Hspec
import Test.QuickCheck as QuickCheck

import qualified Concordium.Crypto.SHA256 as SHA256
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Types.Tokens

import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.BlockState.ProtocolLevelTokens

-- | Generate a 'TokenRawAmount' value.
genTokenRawAmount :: Gen TokenRawAmount
genTokenRawAmount = TokenRawAmount <$> arbitrary

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

testUpdateTokenState :: Assertion
testUpdateTokenState = runBlobStore $ do
    (idxABC, tokens0) <- createToken configABC =<< emptyPLTPV
    (idxDEF, tokens1) <- createToken configDEF tokens0
    mutableStateABC <- getMutableTokenState idxABC tokens1
    mutableStateDEF <- getMutableTokenState idxDEF tokens1
    hash1 <- getHashM @_ @ProtocolLevelTokensHash tokens1

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
    hash2 <- getHashM @_ @ProtocolLevelTokensHash tokens2
    lift $ assertBool "Hash of tokens1 and tokens2 should not be the same" (hash1 /= hash2)
    hash1' <- getHashM @_ @ProtocolLevelTokensHash tokens1
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
    hash3 <- getHashM @_ @ProtocolLevelTokensHash tokens3
    lift $ assertEqual "Hash of tokens2 and tokens3 should be the same" hash2 hash3

tests :: Spec
tests = describe "GlobalStateTests.ProtocolLevelTokens" $ do
    it "createToken" testCreateToken
    it "setTokenCirculatingSupply" testSetTokenCirculatingSupply
    it "updateTokenState" testUpdateTokenState
