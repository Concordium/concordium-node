{-# LANGUAGE DerivingVia #-}
{-|
Definition of cost functions for the different transactions.

* @SPEC: <$DOCS/Transactions#transaction-cost>
-}
module Concordium.Scheduler.Cost where

import Data.Word

import Concordium.Types
import qualified Concordium.Wasm as Wasm

-- |A newtype wrapper around ByteSize to be able to charge for lookup differently
newtype LookupByteSize = LookupByteSize Wasm.ByteSize
    deriving(Eq, Show, Ord, Num, Real, Enum, Integral) via Wasm.ByteSize

-- * Cost factors

-- | The amount of interpreter energy corresponding to one unit of energy.
interpreterEnergyFactor :: Wasm.InterpreterEnergy
interpreterEnergyFactor = 1000

-- | Convert an energy amount to interpreter energy.
toInterpreterEnergy :: Energy -> Wasm.InterpreterEnergy
toInterpreterEnergy = (* interpreterEnergyFactor) . fromIntegral

-- | Convert interpreter energy to general energy (rounding down).
fromInterpreterEnergy :: Wasm.InterpreterEnergy -> Energy
fromInterpreterEnergy = fromIntegral . (`div` interpreterEnergyFactor)


-- * General cost

-- TODO Check for potential overflows in the 'Energy' datatype when calculating cost.
-- This will probably not happen in practice as the required high parameter values will not be reached.
-- But to be defensive, the functions should return 'maxBound' in case the maximum energy would be
-- exceeded.

-- ** Cost for storage

-- $storage-cost
-- Storage cost is mainly determined by the limits on storage size per block we want to ensure.
-- It must be ensured that the actual storage cost is not higher.
-- TODO The current values are estimates, set them more precisely.

-- | Cost for performing a storage operation.
storeBytesBase :: Energy
storeBytesBase = 5

-- | Cost for storing 100 bytes.
storeBytesPer100Byte :: Energy
storeBytesPer100Byte = 50 -- NB: This number must be positive.

-- | Cost for storing the given number of bytes. It is charged for every started 100 bytes.
storeBytes :: Wasm.ByteSize -> Energy
storeBytes n = storeBytesBase + ((fromIntegral n + 99) `div` 100) * storeBytesPer100Byte

-- | For a given amount of energy, determine the number of bytes that can be stored using that energy.
maxStorage :: Energy -> Wasm.ByteSize
maxStorage e = if e < storeBytesBase then 0
               else fromIntegral $ ((e-storeBytesBase) * 100) `div` storeBytesPer100Byte

-- | Cost for doing a pre-lookup (used to determine the cost for the actual lookup).
-- This is especially important now where potentially (depending on laziness) lookups are performed
-- before we can charge for it with its actual size.
lookupBytesPre :: Energy
lookupBytesPre = 15 -- NOTE: This is currently set quite high, to pay at least something for potential
                    -- expensive lookups that are only paid for after the lookup.

-- | Cost for performing a lookup operation.
lookupBytesBase :: Energy
lookupBytesBase = 0 -- NOTE: This is currently 0 because we use 'lookupBytesPre'.

-- | Cost for looking up 100 bytes.
lookupBytesPer100Byte :: Energy
lookupBytesPer100Byte = 3 -- NB: This number must be positive.

-- | Cost for looking up the given number of bytes. It is charged for every started 100 bytes.
lookupBytes :: LookupByteSize -> Energy
lookupBytes n = lookupBytesBase + ((fromIntegral n + 99) `div` 100) * lookupBytesPer100Byte

-- | For a given amount of energy, determine the number of bytes that can be stored using that energy.
maxLookup :: Energy -> LookupByteSize
maxLookup e = if e < lookupBytesBase then 0
              else fromIntegral $ ((e-lookupBytesBase) * 100) `div` lookupBytesPer100Byte

-- | A bound on the maximum size of a contract state ('Value') in bytes.
-- This is used to calculate the maximum possible energy an instance lookup can cost.
-- maxInstanceSize :: Word64
-- maxInstanceSize = 1000000 -- TODO set based on calculated restrictions

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
lookupModule size = lookupBytes (fromIntegral size)

-- | Cost for linking a term of resulting size 100 (as determined by 'linkWithMaxSize').
linkPer100Size :: Energy
linkPer100Size = 1

-- * Cost for individual transactions / actions

-- TODO Remove this summarization of only part of the transaction's cost.
-- Maybe instead add 'storeModule' to abstract from 'storeBytes'.
-- |Cost to deploy the module (cost for typechecking and storing it), excluding cost for
-- loading dependencies (see 'lookupModule'). Also includes cost for compiling the module.
-- Computed from the serialized size of the module.
deployModule :: Word64 -> Energy
deployModule size =
    typeCheck size
  -- As we store considerably more than the source of the module (also interfaces), we add a factor.
  + storeBytes (2 * fromIntegral size)

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
updateBakerSignKey :: Energy
updateBakerSignKey = 90

-- |Cost to update an account's stake delegate.
-- This is parametrised by the number of smart contract instances
-- owned by the account.
-- TODO This cost is examplary, and a suitable relation for the parameters has to be determined.
updateStakeDelegate :: Int -> Energy
updateStakeDelegate nInstances = 100 + fromIntegral nInstances * 50

-- |Cost to update baker aggregation key
-- The main part here is checking a dlog proof, and that cost is essentially
-- the same as the cost in adding a baker - 3 * updateBakerSignKey.
updateBakerAggregationVerifyKey :: Energy
updateBakerAggregationVerifyKey = 2700

-- |Cost to update baker election key.
-- The underlying computation is the same as for updating the baker or account key.
updateBakerElectionKey :: Energy
updateBakerElectionKey = 90

-- |Cost to update existing account keys. Parametrised by amount of keys to update
-- The cost of this transaction is the cost of deserializing Ed25519 verification keys
-- which means checking that it decodes to a point on the curve.
updateAccountKeys :: Int -> Energy
updateAccountKeys n = 5 * fromIntegral n

-- |Cost to add a given number of account keys.
-- The cost of this transaction is the cost of deserializing Ed25519 verification keys
-- which means checking that it decodes to a point on the curve.
-- Same as for updateAccountKeys
addAccountKeys :: Int -> Energy
addAccountKeys = updateAccountKeys

-- |Cost to remove the given number of account keys.
-- There is no expensive operation related to this transaction, which is why it
-- has no cost except for that of checking the header.
removeAccountKeys :: Int -> Energy
removeAccountKeys _n = 0
