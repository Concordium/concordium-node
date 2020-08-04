{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Concordium.Scheduler.DummyData where

import Concordium.Types
import Concordium.ID.Types

import qualified Concordium.Scheduler.Runner as Runner
import qualified Concordium.Scheduler.Environment as Types

import Concordium.Types.DummyData
import Concordium.ID.DummyData
import System.IO.Unsafe

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

{-# NOINLINE cdi1 #-}
{-# NOINLINE cdi2 #-}
{-# NOINLINE cdi3 #-}
{-# NOINLINE cdi4 #-}
{-# NOINLINE cdi5 #-}
{-# NOINLINE cdi6 #-}
{-# NOINLINE cdi7 #-}
{-# WARNING cdi1 "Do not use in production." #-}
cdi1 :: CredentialDeploymentInformation
cdi1 = unsafePerformIO (readCredential "testdata/credential-1.json")
{-# WARNING cdi2 "Do not use in production." #-}
cdi2 :: CredentialDeploymentInformation
cdi2 = unsafePerformIO (readCredential "testdata/credential-2.json")
{-# WARNING cdi3 "Do not use in production." #-}
cdi3 :: CredentialDeploymentInformation
cdi3 = unsafePerformIO (readCredential "testdata/credential-3.json")
-- credential 4 should have the same reg id as credential 3, so should be rejected
{-# WARNING cdi4 "Do not use in production." #-}
cdi4 :: CredentialDeploymentInformation
cdi4 = unsafePerformIO (readCredential "testdata/credential-4.json")
-- Credentials 5 and 6 should have the same account address
{-# WARNING cdi5 "Do not use in production." #-}
cdi5 :: CredentialDeploymentInformation
cdi5 = unsafePerformIO (readCredential "testdata/credential-5.json")
{-# WARNING cdi6 "Do not use in production." #-}
cdi6 :: CredentialDeploymentInformation
cdi6 = unsafePerformIO (readCredential "testdata/credential-6.json")
{-# WARNING cdi7 "Do not use in production." #-}
cdi7 :: CredentialDeploymentInformation
cdi7 = unsafePerformIO (readCredential "testdata/credential-7.json")

{-# WARNING dummyBlockSize "Do not use in production." #-}
dummyBlockSize :: Integer
dummyBlockSize = 10000000000
