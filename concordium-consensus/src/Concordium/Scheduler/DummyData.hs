{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Concordium.Scheduler.DummyData {-# WARNING "This module should not be used in production code." #-} where

import Data.FileEmbed
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Aeson as AE

import Concordium.Common.Version
import Concordium.Types
import Concordium.Scheduler.Types

import qualified Concordium.Scheduler.Runner as Runner

import Concordium.Types.DummyData

-- |Maximum possible expiry of a message.
maxExpiry :: TransactionExpiryTime
maxExpiry = TransactionTime maxBound

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
cdi1 :: AccountCreation
cdi1 = readAccountCreation . BSL.fromStrict $ $(makeRelativeToProject "testdata/credential-1.json" >>= embedFile)
{-# WARNING cdi2 "Do not use in production." #-}
cdi2 :: AccountCreation
cdi2 = readAccountCreation . BSL.fromStrict $ $(makeRelativeToProject "testdata/credential-2.json" >>= embedFile)
{-# WARNING cdi3 "Do not use in production." #-}
cdi3 :: AccountCreation
cdi3 = readAccountCreation . BSL.fromStrict $ $(makeRelativeToProject "testdata/credential-3.json" >>= embedFile)
-- credential 4 should have the same reg id as credential 3, so should be rejected
{-# WARNING cdi4 "Do not use in production." #-}
cdi4 :: AccountCreation
cdi4 = readAccountCreation . BSL.fromStrict $ $(makeRelativeToProject "testdata/credential-4.json" >>= embedFile)
-- Credentials 5 and 6 should have the same account address
{-# WARNING cdi5 "Do not use in production." #-}
cdi5 :: AccountCreation
cdi5 = readAccountCreation . BSL.fromStrict $ $(makeRelativeToProject "testdata/credential-5.json" >>= embedFile)
-- {-# WARNING cdi6 "Do not use in production." #-}
-- cdi6 :: AccountCreation
-- cdi6 = readAccountCreation . BSL.fromStrict $ $(makeRelativeToProject "testdata/credential-6.json" >>= embedFile)
{-# WARNING cdi7 "Do not use in production." #-}
cdi7 :: AccountCreation
cdi7 = readAccountCreation . BSL.fromStrict $ $(makeRelativeToProject "testdata/credential-7.json" >>= embedFile)

readAccountCreation :: BSL.ByteString -> AccountCreation
readAccountCreation bs =
  case AE.eitherDecode bs of
    Left err -> error $ "Cannot read account creation " ++ err
    Right d -> if vVersion d == 0 then vValue d else error "Incorrect account creation version."

{-# WARNING icdi1 "Do not use in production." #-}
icdi1 :: AccountCreation
icdi1 = readAccountCreation . BSL.fromStrict $ $(makeRelativeToProject "testdata/initial-credential-1.json" >>= embedFile)
{-# WARNING icdi2 "Do not use in production." #-}
icdi2 :: AccountCreation
icdi2 = readAccountCreation . BSL.fromStrict $ $(makeRelativeToProject "testdata/initial-credential-2.json" >>= embedFile)
{-# WARNING icdi3 "Do not use in production." #-}
icdi3 :: AccountCreation
icdi3 = readAccountCreation . BSL.fromStrict $ $(makeRelativeToProject "testdata/initial-credential-3.json" >>= embedFile)
{-# WARNING icdi4 "Do not use in production." #-}
icdi4 :: AccountCreation
icdi4 = readAccountCreation . BSL.fromStrict $ $(makeRelativeToProject "testdata/initial-credential-4.json" >>= embedFile)

{-# WARNING dummyBlockSize "Do not use in production." #-}
dummyBlockSize :: Integer
dummyBlockSize = 10000000000

dummyChainMeta :: ChainMetadata
dummyChainMeta = ChainMetadata { slotTime = 0 }

dummyMaxCredentials :: CredentialsPerBlockLimit
dummyMaxCredentials = 10
