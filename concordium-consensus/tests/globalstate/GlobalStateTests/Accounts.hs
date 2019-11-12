{-# LANGUAGE
    FlexibleContexts #-}
module GlobalStateTests.Accounts where

import Prelude hiding (fail)
import Control.Monad hiding (fail)
import Control.Monad.Trans.Except
import Control.Monad.Fail
import Control.Monad.Trans.Reader
import Control.Monad.Trans
import Control.Exception

import Concordium.Types.HashableTo
import qualified Concordium.Crypto.SHA256 as H
import qualified Concordium.ID.Types as ID

import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.GlobalState.Basic.BlockState.Account as B
import qualified Concordium.GlobalState.Basic.BlockState.AccountTable as BAT
import qualified Concordium.GlobalState.Persistent.Account as P
import qualified Concordium.GlobalState.Persistent.AccountTable as PAT
import qualified Concordium.GlobalState.Persistent.Trie as Trie
import Concordium.Types

import Test.Hspec
import Test.HUnit

assertRight :: Either String a -> Assertion
assertRight (Left e) = assertFailure e
assertRight _ = return ()

checkBinary :: (Show a, MonadFail m) => (a -> a -> Bool) -> a -> a -> String -> String -> String -> m ()
checkBinary bop x y sbop sx sy = unless (bop x y) $ fail $ "Not satisfied: " ++ sx ++ " (" ++ show x ++ ") " ++ sbop ++ " " ++ sy ++ " (" ++ show y ++ ")"

-- |Check that a 'B.Accounts' and a 'P.Accounts' are equivalent.
-- That is, they have the same account map, account table, and set of
-- use registration ids.
checkEquivalent :: (MonadBlobStore m BlobRef, MonadFail m) => B.Accounts -> P.Accounts -> m ()
checkEquivalent ba pa = do
    pam <- Trie.toMap (P.accountMap pa)
    checkBinary (==) (B.accountMap ba) pam "==" "Basic account map" "Persistent account map"
    let bat = BAT.toList (B.accountTable ba)
    pat <- PAT.toList (P.accountTable pa)
    checkBinary (==) bat pat "==" "Basic account table (as list)" "Persistent account table (as list)"
    let bath = getHash (B.accountTable ba) :: H.Hash
    let path = getHash (P.accountTable pa) :: H.Hash
    checkBinary (==) bath path "==" "Basic account table hash" "Persistent account table hash"
    (pregids, _) <- P.loadRegIds pa
    checkBinary (==) (B.accountRegIds ba) pregids "==" "Basic registration ids" "Persistent registration ids"

data AccountAction
    = PutAccount Account
    | Exists AccountAddress
    | GetAccount AccountAddress
    | UpdateAccount 
    | UnsafeGetAccount AccountAddress
    | RegIdExists ID.CredentialRegistrationID

runAccountAction :: (MonadBlobStore m BlobRef, MonadFail m) => AccountAction -> (B.Accounts, P.Accounts) -> m (B.Accounts, P.Accounts)
runAccountAction (PutAccount acct) (ba, pa) = do
        let ba' = B.putAccount acct ba
        pa' <- P.putAccount acct pa
        return (ba', pa')




emptyTest :: SpecWith BlobStore
emptyTest = it "empty" $ runReaderT
        (checkEquivalent B.emptyAccounts P.emptyAccounts :: ReaderT BlobStore IO ())
        

tests :: Spec
tests = describe "GlobalStateTests.Accounts" $
    around (bracket createTempBlobStore destroyTempBlobStore) $ do
        emptyTest
