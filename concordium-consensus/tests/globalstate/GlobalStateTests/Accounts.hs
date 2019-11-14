{-# LANGUAGE
    RecordWildCards,
    FlexibleContexts,
    MonoLocalBinds,
    ScopedTypeVariables #-}
module GlobalStateTests.Accounts where

import Prelude hiding (fail)
import Control.Monad hiding (fail)
import Control.Monad.Fail
import Control.Monad.Trans.Reader
import Control.Exception
import qualified Data.Set as Set
import Data.Proxy
import Data.Serialize as S
import Data.Either

import Concordium.Types.HashableTo
import qualified Concordium.Crypto.SHA256 as H
import qualified Concordium.Crypto.SignatureScheme as Sig
import qualified Concordium.ID.Types as ID
import qualified Concordium.ID.Account as ID

import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.GlobalState.Basic.BlockState.Account as B
import qualified Concordium.GlobalState.Basic.BlockState.AccountTable as BAT
import qualified Concordium.GlobalState.Persistent.Account as P
import qualified Concordium.GlobalState.Persistent.AccountTable as PAT
import qualified Concordium.GlobalState.Persistent.Trie as Trie
import Concordium.Types

import Test.Hspec
import Test.HUnit
import Test.QuickCheck

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
    | FlushPersistent
    | ArchivePersistent

randomizeAccount :: AccountAddress -> AccountVerificationKey -> Gen Account
randomizeAccount _accountAddress _accountVerificationKey = do
        _accountNonce <- Nonce <$> arbitrary
        _accountAmount <- Amount <$> arbitrary
        let _accountEncryptedAmount = []
        let _accountEncryptionKey = Nothing
        let _accountCredentials = []
        let _accountStakeDelegate = Nothing
        let _accountInstances = mempty
        return Account{..}

randomActions :: Gen [AccountAction]
randomActions = sized (ra Set.empty)
    where
        randAccount = do
            vk <- Sig.correspondingVerifyKey <$> Sig.genKeyPair
            return (vk, ID.accountAddress vk)
        ra _ 0 = return []
        ra s n = oneof $ [
                putRandAcc,
                exRandAcc,
                getRandAcc,
                (FlushPersistent:) <$> ra s (n-1),
                (ArchivePersistent:) <$> ra s (n-1)
                ] ++ if null s then [] else [putExAcc, exExAcc, getExAcc]        
            where
                putRandAcc = do
                    (vk, addr) <- randAccount
                    acct <- randomizeAccount addr vk
                    (PutAccount acct:) <$> ra (Set.insert (vk, addr) s) (n-1)
                putExAcc = do
                    (vk, addr) <- elements (Set.toList s)
                    acct <- randomizeAccount addr vk
                    (PutAccount acct:) <$> ra s (n-1)
                exRandAcc = do
                    (_, addr) <- randAccount
                    (Exists addr:) <$> ra s (n-1)
                exExAcc = do
                    (_, addr) <- elements (Set.toList s)
                    (Exists addr:) <$> ra s (n-1)
                getRandAcc = do
                    (_, addr) <- randAccount
                    (GetAccount addr:) <$> ra s (n-1)
                getExAcc = do
                    (_, addr) <- elements (Set.toList s)
                    (GetAccount addr:) <$> ra s (n-1)
                


runAccountAction :: (MonadBlobStore m BlobRef, MonadFail m) => AccountAction -> (B.Accounts, P.Accounts) -> m (B.Accounts, P.Accounts)
runAccountAction (PutAccount acct) (ba, pa) = do
        let ba' = B.putAccount acct ba
        pa' <- P.putAccount acct pa
        return (ba', pa')
runAccountAction (Exists addr) (ba, pa) = do
        let be = B.exists addr ba
        pe <- P.exists addr pa
        checkBinary (==) be pe "<->" "account exists in basic" "account exists in persistent"
        return (ba, pa)
runAccountAction (GetAccount addr) (ba, pa) = do
        let bacct = B.getAccount addr ba
        pacct <- P.getAccount addr pa
        checkBinary (==) bacct pacct "==" "account in basic" "account in persistent"
        return (ba, pa)
runAccountAction FlushPersistent (ba, pa) = do
        (_, pa') <- storeUpdate (Proxy :: Proxy BlobRef) pa
        return (ba, pa')
runAccountAction ArchivePersistent (ba, pa) = do
        ppa <- store (Proxy :: Proxy BlobRef) pa
        pa' <- fromRight (error "deserializing blob failed") $ S.runGet (load (Proxy :: Proxy BlobRef)) (S.runPut ppa)
        return (ba, pa')
runAccountAction _ (ba, pa) = return (ba, pa)

emptyTest :: SpecWith BlobStore
emptyTest = it "empty" $ runReaderT
        (checkEquivalent B.emptyAccounts P.emptyAccounts :: ReaderT BlobStore IO ())
        
actionTest :: SpecWith BlobStore
actionTest = it "account actions" $ \bs -> withMaxSuccess 10000 $ property $ do
        acts <- randomActions
        return $ ioProperty $ flip runReaderT bs $ do
            (ba, pa) <- foldM (flip runAccountAction) (B.emptyAccounts, P.emptyAccounts) acts
            checkEquivalent ba pa


tests :: Spec
tests = describe "GlobalStateTests.Accounts" $
    around (bracket createTempBlobStore destroyTempBlobStore) $ do
        emptyTest
        actionTest
