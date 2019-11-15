{-# LANGUAGE
    RecordWildCards,
    TupleSections,
    FlexibleContexts,
    MonoLocalBinds,
    ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
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
import Lens.Micro.Platform

import qualified Data.FixedByteString as FBS
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
    | UpdateAccount AccountAddress (Account -> Account)
    | UnsafeGetAccount AccountAddress
    | RegIdExists ID.CredentialRegistrationID
    | RecordRegId ID.CredentialRegistrationID
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

randomCredential :: Gen ID.CredentialRegistrationID
randomCredential = ID.RegIdCred . FBS.pack <$> vectorOf 42 arbitrary


randomActions :: Gen [AccountAction]
randomActions = sized (ra Set.empty Set.empty)
    where
        randAccount = do
            vk <- Sig.correspondingVerifyKey <$> Sig.genKeyPair
            return (vk, ID.accountAddress vk)
        ra _ _ 0 = return []
        ra s rids n = oneof $ [
                putRandAcc,
                exRandAcc,
                getRandAcc,
                (FlushPersistent:) <$> ra s rids (n-1),
                (ArchivePersistent:) <$> ra s rids (n-1),
                exRandReg,
                recRandReg,
                updateRandAcc
                ] ++ if null s then [] else [putExAcc, exExAcc, getExAcc, unsafeGetExAcc, updateExAcc]
                ++ if null rids then [] else [exExReg, recExReg]
            where
                putRandAcc = do
                    (vk, addr) <- randAccount
                    acct <- randomizeAccount addr vk
                    (PutAccount acct:) <$> ra (Set.insert (vk, addr) s) rids (n-1)
                putExAcc = do
                    (vk, addr) <- elements (Set.toList s)
                    acct <- randomizeAccount addr vk
                    (PutAccount acct:) <$> ra s rids (n-1)
                exRandAcc = do
                    (_, addr) <- randAccount
                    (Exists addr:) <$> ra s rids (n-1)
                exExAcc = do
                    (_, addr) <- elements (Set.toList s)
                    (Exists addr:) <$> ra s rids (n-1)
                getRandAcc = do
                    (_, addr) <- randAccount
                    (GetAccount addr:) <$> ra s rids (n-1)
                getExAcc = do
                    (_, addr) <- elements (Set.toList s)
                    (GetAccount addr:) <$> ra s rids (n-1)
                updateExAcc = do
                    (_, addr) <- elements (Set.toList s)
                    newNonce <- Nonce <$> arbitrary
                    newAmount <- Amount <$> arbitrary
                    let upd acc = if _accountAddress acc == addr
                            then
                                acc {_accountAmount = newAmount, _accountNonce = newNonce}
                            else
                                error "address does not match expected value"
                    (UpdateAccount addr upd:) <$> ra s rids (n-1)
                updateRandAcc = do
                    (vk, addr) <- randAccount
                    let upd _ = error "account address should not exist"
                    if (vk, addr) `Set.member` s then
                        ra s rids n
                    else
                        (UpdateAccount addr upd:) <$> ra s rids (n-1)
                unsafeGetExAcc = do
                    (_, addr) <- elements (Set.toList s)
                    (UnsafeGetAccount addr:) <$> ra s rids (n-1)
                exRandReg = do
                    rid <- randomCredential
                    (RegIdExists rid:) <$> ra s rids (n-1)
                exExReg = do
                    rid <- elements (Set.toList rids)
                    (RegIdExists rid:) <$> ra s rids (n-1)
                recRandReg = do
                    rid <- randomCredential
                    (RecordRegId rid:) <$> ra s (Set.insert rid rids) (n-1)
                recExReg = do -- This is not an expected case in practice
                    rid <- elements (Set.toList rids)
                    (RecordRegId rid:) <$> ra s rids (n-1)

                


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
runAccountAction (UpdateAccount addr upd) (ba, pa) = do
        let ba' = ba & ix addr %~ upd
        (_, pa') <- P.updateAccount (return . ((), ) . upd) addr pa
        return (ba', pa')
runAccountAction (UnsafeGetAccount addr) (ba, pa) = do
        let bacct = B.unsafeGetAccount addr ba
        pacct <- P.unsafeGetAccount addr pa
        checkBinary (==) bacct pacct "==" "account in basic" "account in persistent"
        return (ba, pa)
runAccountAction FlushPersistent (ba, pa) = do
        (_, pa') <- storeUpdate (Proxy :: Proxy BlobRef) pa
        return (ba, pa')
runAccountAction ArchivePersistent (ba, pa) = do
        ppa <- store (Proxy :: Proxy BlobRef) pa
        pa' <- fromRight (error "deserializing blob failed") $ S.runGet (load (Proxy :: Proxy BlobRef)) (S.runPut ppa)
        return (ba, pa')
runAccountAction (RegIdExists rid) (ba, pa) = do
        let be = B.regIdExists rid ba
        (pe, pa') <- P.regIdExists rid pa
        checkBinary (==) be pe "<->" "regid exists in basic" "regid exists in persistent"
        return (ba, pa')
runAccountAction (RecordRegId rid) (ba, pa) = do
        let ba' = B.recordRegId rid ba
        pa' <- P.recordRegId rid pa
        return (ba', pa')

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
