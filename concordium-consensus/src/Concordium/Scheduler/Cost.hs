{-|
Definition of cost functions for the different transactions.

* @SPEC: <$DOCS/Transactions#transaction-cost>
-}
module Concordium.Scheduler.Cost where

import Concordium.Scheduler.Types

import Control.Exception(assert)

-- |The cost to process the header.
-- Processing includes hashing the transaction and checking all signatures against this hash,
-- so transaction size and number of signatures are parameters of this cost.
-- This cost also includes a base cost constant for general work to be done for each transaction.
--
-- This cost will be subtracted from the sender
-- account after the header is checked.
--
-- * @SPEC: <$DOCS/Transactions#transaction-cost-header>
checkHeader
  :: Int -- ^ The number of bytes of serialized transaction header and payload.
  -> Int -- ^ The number of signatures the transaction signature contains.
  -> Energy
checkHeader size nSig =
    6
  + (fromIntegral size) `div` 232
  + (fromIntegral nSig) * 53


-- |Cost to deploy the module. Computed from the serialized size of the module.
-- TODO This cost is outdated and has to be set in relation to the other cost when
-- the respective transactions are enabled.
deployModule :: Int -> Energy
deployModule size = assert (size > 0) (fromIntegral size)

-- |Cost per import. The argument is the number of imports. 
-- NOTE: It might make sense to charge non-linearly, but this might incentivize
-- deep dependencies which we might not want.
-- TODO This cost is outdated and has to be set in relation to the other cost when
-- the respective transactions are enabled.
importedModule :: Int -> Energy
importedModule imports = 10 * fromIntegral imports

-- |Cost to charge when preprocessing a contract init transaction.
-- This includes checking the references to modules, and contracts withing the modules.
-- The cost of this is constant per transaction.
-- TODO This cost is outdated and has to be set in relation to the other cost when
-- the respective transactions are enabled.
initPreprocess :: Energy
initPreprocess = 100

-- |Cost of type-checking and linking the parameters of the init method.
-- Dependent on the serialized size of the parameters.
-- TODO This cost is outdated and has to be set in relation to the other cost when
-- the respective transactions are enabled.
initParamsTypecheck :: Int -> Energy
initParamsTypecheck size = assert (size > 0) (fromIntegral size)

-- |Cost to charge when preprocessing a contract update transaction.
-- This includes checking the references to modules, and contracts withing the modules.
-- The cost of this is constant per transaction.
-- TODO This cost is outdated and has to be set in relation to the other cost when
-- the respective transactions are enabled.
updatePreprocess :: Energy
updatePreprocess = 100

-- |Cost of type-checking and linking the message of the init method. Dependent
-- on the serialized size of the parameters. Note that this is only explicitly
-- charged for the top-level update. Inter-contract messages are known to be
-- type-correct and are already in linked/compiled form.
-- TODO This cost is outdated and has to be set in relation to the other cost when
-- the respective transactions are enabled.
updateMessageTypecheck :: Int -> Energy
updateMessageTypecheck size = assert (size > 0) (fromIntegral size)

-- |Fixed cost per generated inter-contract message.
-- TODO This cost is outdated and has to be set in relation to the other cost when
-- the respective transactions are enabled.
interContractMessage :: Energy
interContractMessage = 10

-- |Cost to transfer to an account.
transferAccount :: Energy
transferAccount = 0

-- |Cost to add a credential to an account. This cost is costant regardless of
-- the details of the data. This might change.
deployCredential :: Energy
deployCredential = 35000

-- |Cost to add an encryption key to an account.
-- TODO This cost is outdated and has to be set in relation to the other cost when
-- the respective transactions are enabled.
deployEncryptionKey :: Energy
deployEncryptionKey = 500

-- |Cost to register a new baker.
addBaker :: Energy
addBaker = 3000

-- |Cost to remove a baker.
removeBaker :: Energy
removeBaker = 0

-- |Cost to update the baker's reward account.
updateBakerAccount :: Energy
updateBakerAccount = 90

-- |Cost to update the baker's signature verification key.
updateBakerKey :: Energy
updateBakerKey = 90

-- |Cost to update an account's stake delegate.
-- This is parametrised by the number of smart contract instances
-- owned by the account.
-- TODO This cost is outdated and has to be set in relation to the other cost when
-- the respective transactions are enabled.
updateStakeDelegate :: Int -> Energy
updateStakeDelegate nInstances = 100 + fromIntegral nInstances * 50
