{-# LANGUAGE DerivingVia #-}
{-|
Definition of cost functions for the different transactions.

* @SPEC: <$DOCS/Transactions#transaction-cost>
-}
module Concordium.Scheduler.Cost where

import Concordium.Types
import Concordium.ID.Types as ID
import qualified Concordium.Wasm as Wasm
import Data.Word

-- |A class to convert to and from 'Energy' used by the scheduler.
-- The function should satisfy
--
--   * @toEnergy (fromEnergy x) <= x@
class ResourceMeasure a where
  toEnergy :: a -> Energy
  fromEnergy :: Energy -> a

instance ResourceMeasure Energy where
  {-# INLINE toEnergy #-}
  toEnergy = id
  {-# INLINE fromEnergy #-}
  fromEnergy = id

-- |Measures the cost of running the interpreter.
instance ResourceMeasure Wasm.InterpreterEnergy where
  {-# INLINE toEnergy #-}
  toEnergy = fromInterpreterEnergy
  {-# INLINE fromEnergy #-}
  fromEnergy = toInterpreterEnergy

-- |Measures the cost of __storing__ the given amount of bytes of smart contract
-- state.
instance ResourceMeasure Wasm.ByteSize where
  {-# INLINE toEnergy #-}
  toEnergy = fromIntegral
  {-# INLINE fromEnergy #-}
  fromEnergy = fromIntegral

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


-- * Costs for top-level account transactions

-- |The NRG cost is assigned according to the formula A * numSignatures + B *
-- size + C_t where C_t is transaction specific cost and A and B are transaction
-- independent factors. 
-- 

-- |The A constant for NRG assignment.
constA :: Energy
constA = 100

-- |The B constant for NRG assignment.
constB :: Energy
constB = 1

-- |C_t for simple transfer.
simpleTransferCost :: Energy
simpleTransferCost = 300

-- |C_t for encrypted transfer
encryptedTransferCost :: Energy
encryptedTransferCost = 27000

-- |C_t for transfer from public to encrypted
transferToEncryptedCost :: Energy
transferToEncryptedCost = 600

-- |C_t for transfer from encrypted to public
transferToPublic :: Energy
transferToPublic = 14850

-- |C_t for transfer with schedule. The argument is the number of releases.
scheduledTransferCost :: Int -> Energy
scheduledTransferCost n = fromIntegral n * (300 + 64)

-- |C_t for adding a baker
addBakerCost :: Energy
addBakerCost = 4050

-- |C_t for updating baker keys
updateBakerKeysCost :: Energy
updateBakerKeysCost = 4050

-- |C_t for updating baker stake.
updateBakerStakeCost :: Energy
updateBakerStakeCost = 300

-- |C_t for updating baker automatic restake option
updateBakerRestakeCost :: Energy
updateBakerRestakeCost = 300

-- |C_t for removing a baker
removeBakerCost :: Energy
removeBakerCost = 300

-- |C_t for updating account credentials
updateCredentialsCost ::
  [Int] -- ^ A list of keys attached to each new credential.
  -> Energy
updateCredentialsCost = sum . map (deployCredential ID.Normal)

-- |C_t for deploying a Wasm module.
-- The argument is the size of the Wasm module in bytes.
deployModuleCost :: Word64 -> Energy
deployModuleCost size = fromIntegral size `div` 10

-- |C_t for initializing a contract instance.
initializeContractInstanceCost ::
  Wasm.InterpreterEnergy -- ^ How much energy it took to execute the initialization code.
  -> Word64 -- ^ Size in bytes of the smart contract module that the instance is created from.
  -> Maybe Wasm.ByteSize -- ^ Size of the initial smart contract state if initialization succeeded.
  -> Energy
initializeContractInstanceCost ie ms ss =
  lookupModule ms + toEnergy ie + maybe 0 ((initializeContractInstanceCreateCost +) . toEnergy) ss + initializeContractInstanceBaseCost

-- |C_t for updating smart contract state.
-- This will be applied to each smart contract that is affected by the transaction.
updateContractInstanceCost ::
  Wasm.InterpreterEnergy -- ^ How much energy it t ook to execute the update code.
  -> Word64 -- ^ Size in bytes of the module the contract code belongs to.
  -> Wasm.ByteSize -- ^ Size of the original state.
  -> Maybe Wasm.ByteSize -- ^ Size of the new state, if update was successful.
  -> Energy
updateContractInstanceCost ie ms se ss =
  lookupModule ms + lookupContractState se + toEnergy ie + maybe 0 toEnergy ss + updateContractInstanceBaseCost

-- |C_t for updating existing credential keys. Parametrised by amount of new keys.
updateCredentialKeysCost :: Int -> Energy
updateCredentialKeysCost numKeys = 100 * fromIntegral numKeys

-- * NRG assignments for non-account transactions.

-- |NRG value of adding a credential to an account. This cost is costant
-- regardless of the details of the data. It is not charged directly, but it is accounted for
-- in block energy.
deployCredential ::
  ID.CredentialType -- ^ Type of the credential. Initial credentials are cheaper.
  -> Int -- ^Number of keys belonging to the credential.
  -> Energy
deployCredential ID.Initial numKeys = 1000 + 100 * fromIntegral numKeys
deployCredential ID.Normal numKeys = 54000 + 100 * fromIntegral numKeys

-- * Auxiliary definitions

-- |The cost A * numKeys + B * size
baseCost ::
  Word64 -- ^ The size of the transaction body in bytes.
  -> Int -- ^ The number of keys that signed the transaction.
  -> Energy
baseCost size numKeys = constA * fromIntegral numKeys + constB * fromIntegral size

-- |Fixed cost per generated inter-contract message.
interContractMessage :: Energy
interContractMessage = 10

-- |Cost of looking up a contract instance with a given state.
lookupContractState :: Wasm.ByteSize -> Energy
lookupContractState ss = fromIntegral ss `div` 50

lookupModule :: Word64 -> Energy
lookupModule ms = fromIntegral ms `div` 50

-- | The base cost of initializing a contract instance to cover administrative costs.
-- Even if no code is run and no instance created.
initializeContractInstanceBaseCost :: Energy
initializeContractInstanceBaseCost = 300

-- |Cost of creating an empty smart contract instance.
initializeContractInstanceCreateCost :: Energy
initializeContractInstanceCreateCost = 200

-- | The base cost of updating a contract instance to cover administrative
-- costs. Even if no code is run.
updateContractInstanceBaseCost :: Energy
updateContractInstanceBaseCost = 300
