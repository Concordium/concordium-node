{-|
Definition of cost functions for the different transactions.

* @SPEC: <$DOCS/Transactions#transaction-cost>
-}
module Concordium.Scheduler.Cost where

import Prelude hiding (lookup)

import Data.Word

import Concordium.Scheduler.Types


-- * Cost factors

-- | The amount of interpreter energy corresponding to one unit of energy.
interpreterEnergy :: Energy
interpreterEnergy = 100 -- TODO find suitable factor

-- | Convert an energy amount to interpreter energy.
toInterpreterEnergy :: Energy -> Energy
toInterpreterEnergy = (* interpreterEnergy)

-- | Convert interpreter energy to general energy (rounding down).
fromInterpreterEnergy :: Energy -> Energy
fromInterpreterEnergy = (`div` interpreterEnergy)


-- * General cost

-- FIXME Some costs with parameters are currently prone to overflow in the 'Energy' datatype.
-- This will probably not happen in practice as the required high parameter values will not be reached.
-- But to be defensive, the functions should return 'maxBound' in case the maximum energy would be
-- exceeded.

-- ** Cost for storage

-- $storage-cost
-- Storage cost is mainly determined by the limits on storage size per block we want to ensure.
-- It must be ensured that the actual storage cost is not higher.
-- TODO The current values are estimates, set them more precisely.

-- | Cost for performing a storage operation.
storageBase :: Energy
storageBase = 5

-- | Cost for storing 100 bytes.
storagePer100Byte :: Energy
storagePer100Byte = 5

-- | Cost for storing the given number of bytes. It is charged for every started 100 bytes.
storage :: Word64 -> Energy
storage n = storageBase + ((fromIntegral n + 99) `div` 100) * storagePer100Byte

-- | For a given amount of energy, determine the number of bytes that can be stored using that energy.
maxStorage :: Energy -> Word64
maxStorage e = fromIntegral $ ((max 0 (e-storageBase)) * 100) `div` storagePer100Byte

-- | Cost for performing a lookup operation.
lookupBase :: Energy
lookupBase = 3

-- | Cost for looking up 100 bytes.
lookupPer100Byte :: Energy
lookupPer100Byte = 3

-- | Cost for looking up the given number of bytes. It is charged for every started 100 bytes.
lookup :: Word64 -> Energy
lookup n = lookupBase + ((fromIntegral n + 99) `div` 100) * lookupPer100Byte

-- | For a given amount of energy, determine the number of bytes that can be stored using that energy.
maxLookup :: Energy -> Word64
maxLookup e = fromIntegral $ ((max 0 (e-lookupBase)) * 100) `div` lookupPer100Byte

-- | A bound on the maximum size of a contract state ('Value') in bytes.
-- This is used to calculate the maximum possible energy an instance lookup can cost.
maxInstanceSize :: Word64
maxInstanceSize = 1000000 -- TODO set based on calculated restrictions

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
  :: Word64 -- ^ The number of bytes of serialized transaction header and payload.
  -> Int -- ^ The number of signatures the transaction signature contains.
  -> Energy
checkHeader size nSig =
    6
  + (fromIntegral size) `div` 232
  + (fromIntegral nSig) * 53

-- |Cost for type checking a module or term based on its serialized size (excluding cost for
-- loading dependencies, see 'lookupModule'). This also includes cost for compiling.
typeCheck :: Word64 -> Energy
typeCheck size = (fromIntegral size) * 3 -- TODO find factor

-- |Cost per import when typechecking a module. The argument is the size of the
-- imported module as specified by its 'Interface'.
-- NOTE: It might make sense to charge non-linearly, but this might incentivize
-- deep dependencies which we might not want.
lookupModule :: Word64 -> Energy
lookupModule size = lookup size

-- | Cost for linking a term of resulting size 100 (as determined by 'linkWithMaxSize').
linkPer100Size :: Energy
linkPer100Size = 1

-- | Cost for linking a term of the given resulting size (as determined by 'linkWithMaxSize').
link :: Word64 -> Energy
link size = ((fromIntegral size + 99) `div` 100) * linkPer100Size

-- | For a given amount of energy, determine the maximum resulting size of a tearm that can be
-- linked using that energy.
maxLink :: Energy -> Word64
maxLink e = fromIntegral $ (e * 100) `div` linkPer100Size

-- * Cost for individual transactions / actions

-- |Cost to deploy the module (cost for typechecking and storing it), excluding cost for
-- loading dependencies (see 'lookupModule'). Also includes cost for compiling the module.
-- Computed from the serialized size of the module.
deployModule :: Word64 -> Energy
deployModule size =
    typeCheck size
  -- As we store considerably more than the source of the module (also interfaces), we add a factor.
  + storage (2*size)

-- |Cost of type-checking and linking the parameters of the init method.
-- Dependent on the serialized size of the parameters.
initParamsTypecheck :: Word64 -> Energy
initParamsTypecheck = typeCheck

-- |Cost of type-checking and linking the message of the init method. Dependent
-- on the serialized size of the parameters. Note that this is only explicitly
-- charged for the top-level update. Inter-contract messages are known to be
-- type-correct and are already in linked/compiled form.
updateMessageTypecheck :: Word64 -> Energy
updateMessageTypecheck = typeCheck

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

-- |Cost to update the election difficulty.
updateElectionDifficulty :: Energy
updateElectionDifficulty = 0
