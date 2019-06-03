module Concordium.Scheduler.Cost where

import Concordium.Scheduler.Types

import Control.Exception(assert)

-- |The cost to process the header. This cost will be subtracted from the sender
-- account after the header is checked.
checkHeader :: Energy
checkHeader = 100

-- |Minimal deposit needed in order to start processing the transaction.
-- We should have this since checking the header is non-trivial cost.
minimumDeposit :: Amount
minimumDeposit = 100

-- |Cost to deploy the module. Computed from the serialized size of the module.
deployModule :: Int -> Energy
deployModule size = assert (size > 0) (fromIntegral size)

-- |Cost per import. The argument is the number of imports. 
-- NOTE: It might make sense to charge non-linearly, but this might incentivize
-- deep dependencies which we might not want.
importedModule :: Int -> Energy
importedModule imports = 10 * fromIntegral imports

-- |Cost to charge when preprocessing a contract init transaction.
-- This includes checking the references to modules, and contracts withing the modules.
-- The cost of this is constant per transaction.
initPreprocess :: Energy
initPreprocess = 100

-- |Cost of type-checking and linking the parameters of the init method.
-- Dependent on the serialized size of the parameters.
initParamsTypecheck :: Int -> Energy
initParamsTypecheck size = assert (size > 0) (fromIntegral size)

-- |Cost to charge when preprocessing a contract update transaction.
-- This includes checking the references to modules, and contracts withing the modules.
-- The cost of this is constant per transaction.
updatePreprocess :: Energy
updatePreprocess = 100

-- |Cost of type-checking and linking the message of the init method. Dependent
-- on the serialized size of the parameters. Note that this is only explicitly
-- charged for the top-level update. Inter-contract messages are known to be
-- type-correct and are already in linked/compiled form.
updateMessageTypecheck :: Int -> Energy
updateMessageTypecheck size = assert (size > 0) (fromIntegral size)

-- |Fixed cost per generated inter-contract message.
interContractMessage :: Energy
interContractMessage = 10

-- |Cost to add a credential to an account. This cost is costant regardless of
-- the details of the data. This might change.
deployCredential :: Energy
deployCredential = 3000


-- |Cost to add an encryption key to an account.
deployEncryptionKey :: Energy
deployEncryptionKey = 500
