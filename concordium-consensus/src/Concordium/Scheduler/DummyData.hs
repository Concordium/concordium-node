{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Concordium.Scheduler.DummyData where

import Data.FileEmbed
import qualified Data.ByteString.Lazy as BSL

import Concordium.Types
import Concordium.ID.Types

import qualified Concordium.Scheduler.Runner as Runner

import Concordium.Types.DummyData
import Concordium.ID.DummyData

-- Make a header assuming there is only one key on the account, its index is 0
{-# WARNING makeHeaderWithExpiry "Do not use in production." #-}
makeHeaderWithExpiry :: AccountAddress -> Nonce -> Energy -> TransactionExpiryTime -> Runner.TransactionHeader
makeHeaderWithExpiry = Runner.TransactionHeader

-- NB: In order for tests to work, the slot time (currently set to 0)
-- must be <= than transaction expiry time (currently also set to 0
-- in `dummyTransactionExpiryTime`)
{-# WARNING makeDummyHeader "Do not use in production." #-}
makeDummyHeader :: AccountAddress -> Nonce -> Energy -> Runner.TransactionHeader
makeDummyHeader a n e = makeHeaderWithExpiry a n e dummyMaxTransactionExpiryTime

{-# WARNING cdi1 "Do not use in production." #-}
cdi1 :: CredentialDeploymentInformation
cdi1 = readCredential . BSL.fromStrict $ $(makeRelativeToProject "testdata/credential-1.json" >>= embedFile)
{-# WARNING cdi2 "Do not use in production." #-}
cdi2 :: CredentialDeploymentInformation
cdi2 = readCredential . BSL.fromStrict $ $(makeRelativeToProject "testdata/credential-2.json" >>= embedFile)
{-# WARNING cdi3 "Do not use in production." #-}
cdi3 :: CredentialDeploymentInformation
cdi3 = readCredential . BSL.fromStrict $ $(makeRelativeToProject "testdata/credential-3.json" >>= embedFile)
-- credential 4 should have the same reg id as credential 3, so should be rejected
{-# WARNING cdi4 "Do not use in production." #-}
cdi4 :: CredentialDeploymentInformation
cdi4 = readCredential . BSL.fromStrict $ $(makeRelativeToProject "testdata/credential-4.json" >>= embedFile)
-- Credentials 5 and 6 should have the same account address
{-# WARNING cdi5 "Do not use in production." #-}
cdi5 :: CredentialDeploymentInformation
cdi5 = readCredential . BSL.fromStrict $ $(makeRelativeToProject "testdata/credential-5.json" >>= embedFile)
{-# WARNING cdi6 "Do not use in production." #-}
cdi6 :: CredentialDeploymentInformation
cdi6 = readCredential . BSL.fromStrict $ $(makeRelativeToProject "testdata/credential-6.json" >>= embedFile)
{-# WARNING cdi7 "Do not use in production." #-}
cdi7 :: CredentialDeploymentInformation
cdi7 = readCredential . BSL.fromStrict $ $(makeRelativeToProject "testdata/credential-7.json" >>= embedFile)

{-# WARNING icdi1 "Do not use in production." #-}
icdi1 :: InitialCredentialDeploymentInfo
icdi1 = readInitialCredential . BSL.fromStrict $ $(makeRelativeToProject "testdata/initial-credential-1.json" >>= embedFile)
{-# WARNING icdi2 "Do not use in production." #-}
icdi2 :: InitialCredentialDeploymentInfo
icdi2 = readInitialCredential . BSL.fromStrict $ $(makeRelativeToProject "testdata/initial-credential-2.json" >>= embedFile)
{-# WARNING icdi3 "Do not use in production." #-}
icdi3 :: InitialCredentialDeploymentInfo
icdi3 = readInitialCredential . BSL.fromStrict $ $(makeRelativeToProject "testdata/initial-credential-3.json" >>= embedFile)
{-# WARNING icdi4 "Do not use in production." #-}
icdi4 :: InitialCredentialDeploymentInfo
icdi4 = readInitialCredential . BSL.fromStrict $ $(makeRelativeToProject "testdata/initial-credential-4.json" >>= embedFile)

{-# WARNING dummyBlockSize "Do not use in production." #-}
dummyBlockSize :: Integer
dummyBlockSize = 10000000000
