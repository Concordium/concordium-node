{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RecordWildCards #-}
module Concordium.GlobalState.Persistent.Account where
{-
import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import Data.Word

import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Types
import Concordium.GlobalState.Persistent.Types
import Concordium.ID.Types
import Concordium.GlobalState.Persistent.Trie
-}
{-
share [mkPersist sqlSettings, mkMigrate "migrateAccount"] [persistLowerCase|
PAccount
    hash Hash.Hash
    address AccountAddress
    nonce Nonce
    amount Amount
    encryptedAmount [EncryptedAmount]
    encyptionKey AccountEncryptionKey Maybe
    verificationKey AccountVerificationKey
    signatureScheme SchemeId
    credentials [CredentialDeploymentValues]
    stakeDelegate BakerId Maybe
    accountInstances [ContractAddress]
    UniqueHash hash
    deriving Show
PAccountTableEntry
    level Word8
    full Bool
    hash Hash.Hash
    leftChild PAccountTableEntryId Maybe
    rightChild PAccountTableEntryId Maybe
    leftLeaf PAccount Maybe
    rightLeft PAccount Maybe
    UniqueHash hash
    deriving Show
|]

data PAccountTable = PATTree !PAccountTableEntryId | PATEmpty | PATAccount !PAccountId

data ActiveAccounts = ActiveAccounts {
    accountMap :: !(Map.Map AccountAddress AccountIndex),
    accountTable :: !PAccountTable,
    accountRegIds :: !(Set.Set ID.CredentialRegistrationID)
}

newtype RetiredAccounts = RetiredAccounts PAccountTable

retireAccounts :: ActiveAccounts -> RetiredAccounts
retireAccounts = RetiredAccounts . accountTable
-} 
{-
reactivateAccounts :: RetiredAccounts -> PersistM ActiveAccounts
reactivateAccounts (RetiredAccounts accountTable) = do

    return $! ActiveAccounts{..}
-}

{-
main :: IO ()
main = runSqlite ":memory:" $ do
    runMigration migrateAll

    johnId <- insert $ Person "John Doe" $ Just 35
    janeId <- insert $ Person "Jane Doe" Nothing

    insert $ BlogPost "My fr1st p0st" johnId
    insert $ BlogPost "One more for good measure" johnId

    oneJohnPost <- selectList [BlogPostAuthorId ==. johnId] [LimitTo 1]
    liftIO $ print (oneJohnPost :: [Entity BlogPost])

    john <- get johnId
    liftIO $ print (john :: Maybe Person)

    delete janeId
    deleteWhere [BlogPostAuthorId ==. johnId]
    -}