{-# LANGUAGE DerivingVia #-}
{-|
Definition of cost functions for the different transactions.

* @SPEC: <$DOCS/Transactions#transaction-cost>
-}
module Concordium.Scheduler.Cost where

import Data.Word

import Concordium.Types
import Concordium.ID.Types as ID
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
storeBytesPer100Byte = 50 -- NB: This number must not be 0.

-- | Cost for storing the given number of bytes. It is charged for every started 100 bytes.
storeBytes :: Wasm.ByteSize -> Energy
storeBytes n = storeBytesBase + ((fromIntegral n + 99) `div` 100) * storeBytesPer100Byte

-- | Cost for storing the module of a given size.
storeModule :: Wasm.ByteSize -> Energy
-- the factor 2 is because we store about twice the size of the module.
-- We need to store the source for API lookups, and the processed artifact.
storeModule n = storeBytesBase + 2 * ((fromIntegral n + 99) `div` 100) * storeBytesPer100Byte

-- | For a given amount of energy, determine the number of bytes that can be stored using that energy.
maxStorage :: Energy -> Wasm.ByteSize
maxStorage e = if e <= storeBytesBase then 0
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
-- |Cost to deploy the module (cost for validating, processing, and storing it).
-- Computed from the serialized size of the module.
deployModule :: Word64 -> Energy
deployModule size =
  -- the div 30 is derived from benchmarks, which indicate that processing
  -- is a linear function of the input, with this factor.
    fromIntegral (size `div` 30)
  -- As we store about twice the size of the source of the module we add a factor.
  + storeModule (fromIntegral size)

-- |Fixed cost per generated inter-contract message.
interContractMessage :: Energy
interContractMessage = 10

-- |Cost to transfer to an account.
transferAccount :: Energy
transferAccount = 0

-- |Cost to add a credential to an account. This cost is costant regardless of
-- the details of the data. This might change.
deployCredential :: ID.CredentialType -> Energy
deployCredential ID.Initial = 1000
deployCredential ID.Normal = 35000

-- |Cost to register a new baker.
addBaker :: Energy
addBaker = 3000

-- |Cost to remove a baker.
removeBaker :: Energy
removeBaker = 0

-- |Cost to update a baker's stake.
updateBakerStake :: Energy
updateBakerStake = 90

-- |Cost to update whether a baker restakes its earnings
updateBakerRestakeEarnings :: Energy
updateBakerRestakeEarnings = 0

-- |Cost to update a baker's keys.
updateBakerKeys :: Energy
updateBakerKeys = 2980

-- |Cost to update existing credential keys. Parametrised by amount of new keys.
-- The cost of this transaction is the cost of deserializing Ed25519 verification keys
-- which means checking that it decodes to a point on the curve.
updateCredentialKeys :: Int -> Energy
updateCredentialKeys n = 5 * fromIntegral n

-- |Cost of an encrypted amount transfer validation.
encryptedAmountTransfer :: Energy
encryptedAmountTransfer = 30000

-- |Cost of an public to secret transfer
-- FIXME: After we have benchmarked proof checking update this.
pubToSecTransfer :: Energy
pubToSecTransfer = 100

-- |Cost of secret to public transfer.
-- Benchmarking shows that this takes a bit more than half the time the
-- encrypted amount transfer.
secToPubTransfer :: Energy
secToPubTransfer = 16000


-- |Cost of executing a transfer with schedule.
-- Linear cost on the number of releases.
transferWithSchedule :: Int -> Energy
transferWithSchedule numReleases = 100 * fromIntegral numReleases
