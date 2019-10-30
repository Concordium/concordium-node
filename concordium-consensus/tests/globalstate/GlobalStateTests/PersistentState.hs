{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module GlobalStateTests.PersistentState where

import Data.Word
import Control.Monad.IO.Class
import Control.Monad.RWS.Strict
import Data.Proxy
import Data.Serialize
import Data.IORef
import Data.Maybe
import System.Random
import Control.Monad

import Test.QuickCheck
import Test.Hspec

import qualified Concordium.Crypto.SHA256 as Hash
import qualified Concordium.Crypto.SignatureScheme as SigScheme
import qualified Concordium.Crypto.BlockSignature as Sig

import Acorn.Utils.Init

import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Basic.TreeState
import Concordium.GlobalState.Persistent.BlockState
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Bakers
import Concordium.GlobalState.SeedState
import Concordium.Types


data PersistentContext = PersistentContext {
    pcBlobStore :: !BlobStore,
    pcModuleCache :: !(IORef ModuleCache)
}

instance HasBlobStore PersistentContext where
    blobStore = blobStore . pcBlobStore

instance HasModuleCache PersistentContext where
    moduleCache = pcModuleCache

cryptoParams :: CryptographicParameters
cryptoParams = fromJust $ readCryptographicParameters "{  \"dLogBaseChain\": \"97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb\", \"onChainCommitmentKey\": \"0000000199e4a085f8d083de689f79e5b296593644037499db92534071d1d5d607fe8594c398442ef20445a8eafae6695c4ed4a3b38a61d0ddd52fae990294114a2c2d20705c868bc979a07ccece02234b5b2f60a16edf7a17b676be108442417aecf34d\" }"

emptyBirkParameters :: BirkParameters
emptyBirkParameters = BirkParameters 0.5 emptyBakers (genesisSeedState (Hash.hash "blah") 360)
{-
myEmptyBlockState :: PersistentBlockState
myEmptyBlockState = emptyBlockState emptyBirkParameters cryptoParams

makeAccount :: Int -> Account
makeAccount seed = acct {_accountAmount = 1000000000000, _accountStakeDelegate = Nothing}
  where
    acct = newAccount (Sig.verifyKey kp) SigScheme.Ed25519
    kp = fst (Sig.randomKeyPair (mkStdGen seed))


tests :: Spec
tests = describe "GlobalStateTests.PersistentState" $ do
    it "empty state" $ do
        runBlobStoreTemp "." $ do
            let pbs = myEmptyBlockState
            z0 <- doGetExecutionCost pbs
            pbs' <- uncacheBuffered pbs
            z <- doGetExecutionCost pbs'
            liftIO $ z `shouldBe` z0
    it "one account" $ do
        runBlobStoreTemp "." $ do
            let pbs = myEmptyBlockState
            let testAcc = makeAccount 1
            (b, pbs0) <- doPutNewAccount pbs testAcc
            unless b $ liftIO $ expectationFailure "Failed to put account"
            pbs' <- uncacheBuffered pbs0
            Just readAcc <- doGetAccount pbs' (_accountAddress testAcc)
            liftIO $ readAcc `shouldBe` testAcc
    it "one module" $ do
        runBlobStoreTemp "." $ do
            let pbs = myEmptyBlockState
            let (_, _, pms) = baseState
            let ((mref, iface, viface, src):_) = moduleList pms
            (b, pbs0) <- doPutNewModule pbs mref iface viface src
            unless b $ liftIO $ expectationFailure "Failed to put module"
            pbs' <- uncacheBuffered pbs0
            modList <- doGetModuleList pbs'
            liftIO $ modList `shouldBe` [mref]

-}
tests :: Spec
tests = return ()