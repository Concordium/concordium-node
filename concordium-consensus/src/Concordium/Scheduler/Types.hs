{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}

module Concordium.Scheduler.Types (module Concordium.Scheduler.Types,
                                   module Concordium.GlobalState.Types) where

import Data.HashMap.Strict(HashMap)
import qualified Data.HashMap.Strict as Map

import Prelude hiding(fail)

import qualified Data.Sequence as Seq

import Control.Monad.Reader

import qualified Data.Serialize.Put as P
import qualified Data.Serialize.Get as G
import qualified Data.Serialize as S

import qualified Acorn.Core as Core
import Concordium.GlobalState.Types
import Concordium.GlobalState.Acorn.Interfaces
import qualified Concordium.ID.Types as IDTypes

{-# INLINE pushToStack #-}
pushToStack :: RTEnv -> Value -> RTEnv
pushToStack env v = env { localStack = v Seq.<| localStack env }

{-# INLINE pushAllToStack #-}
pushAllToStack :: RTEnv -> Seq.Seq Value -> RTEnv
pushAllToStack env v = env { localStack = v Seq.>< localStack env }

emptyStack :: RTEnv
emptyStack = RTEnv Seq.empty

-- |NB: This method is unsafe and will raise an error if the stack is empty.
{-# INLINE peekStack #-}
peekStack :: RTEnv -> Value
peekStack env = localStack env `Seq.index` 0

-- |NB: This method is unsafe and will raise an error if the stack is empty.
{-# INLINE peekStack' #-}
peekStack' :: RTEnv -> Value
peekStack' env = localStack env `Seq.index` 1

{-# INLINE peekStack'' #-}
peekStack'' :: RTEnv -> Value
peekStack'' env = localStack env `Seq.index` 2

{-# INLINE peekStack''' #-}
peekStack''' :: RTEnv -> Value
peekStack''' env = localStack env `Seq.index` 3

{-# INLINE peekStackK #-}
peekStackK :: RTEnv -> Int -> Value
peekStackK env k = localStack env `Seq.index` k

-- |These are the messages that are generated as parts of contract execution.
data InternalMessage = TSend !ContractAddress !Amount !Value | TSimpleTransfer !Address !Amount

-- t -> msgty of the contract
type SenderTy = Expr
-- just a state view function, stateTy of the contract -> specified type
type GetterTy = Expr

-- aliases for the types of init and update/receive methods.
type InitType = Expr
type UpdateType = Expr


data Instance = Instance
    {ref :: !ContractAddress                 -- ^Address of this contract instance.
    ,ireceiveFun :: !Expr                         -- ^Pointer to its receive function.
    ,iModuleIface :: !(Interface, ValueInterface)
    ,imsgTy :: !(Core.Type Core.ModuleRef)        -- ^The type of messages its receive function supports.
    ,lState :: !Value                     -- ^The current local state of the instance.
    ,iamount :: !Amount                   -- ^And the amount of GTUs it currently owns.
    ,instanceImplements :: !(HashMap (Core.ModuleRef, Core.TyName) ImplementsValue)  
    -- ^Implementation of the given class sender method. This can also be looked
    -- up through the contract, and we should probably do that, but having it
    -- here simplifies things.
    }

-- * Types related to block execution, e.g, transactions, blocks, etc.

-- |Additional data needed specifically by the init method of the contract.
-- The data here should match with the InitContext structure defined in Blockchain.acorn
data InitContext = InitContext
    {initOrigin :: !AccountAddress}

-- |Additional data needed specifically by the receive method of the contract.
-- The data here should match with the InitContext structure defined in Blockchain.acorn
data ReceiveContext = ReceiveContext
    {invoker :: !AccountAddress
    ,selfAddress :: !ContractAddress
    }

-- |The transaction payload. Currently only 4 transaction kinds are supported.
data Payload = DeployModule !Core.Module   -- put module on the chain
             | InitContract !Amount !Core.ModuleRef !Core.TyName !(Core.Expr Core.ModuleName)   -- ^Call init method of contract.
             | Update !Amount !ContractAddress !(Core.Expr Core.ModuleName) -- ^The last argument is the parameter.
             | Transfer !Address !Amount     -- ^Where (which can be a contract) and what amount to transfer.
             | CreateAccount !IDTypes.AccountCreationInformation
--              deriving(Show)

instance S.Serialize Payload where
  put (DeployModule amod) =
    P.putWord8 0 <>
    Core.putModule amod
  put (InitContract amnt mref cname params) =
      P.putWord8 1 <>
      S.put amnt <>
      Core.putModuleRef mref <>
      Core.putTyName cname <>
      Core.putExpr params
  put (Update amnt cref msg) =
    P.putWord8 2 <>
    S.put amnt <>
    S.put cref <>
    Core.putExpr msg
  put (Transfer addr amnt) =
    P.putWord8 3 <>
    S.put addr <>
    S.put amnt
  put (CreateAccount aci) =
    P.putWord8 4 <>
    S.put aci

  get = do
    h <- G.getWord8
    case h of
      0 -> DeployModule <$> Core.getModule
      1 -> InitContract <$> S.get <*> Core.getModuleRef <*> Core.getTyName <*> Core.getExpr
      2 -> Update <$> S.get <*> S.get <*> Core.getExpr
      3 -> Transfer <$> S.get <*> S.get
      4 -> CreateAccount <$> S.get
      _ -> fail "Only 5 types of transactions types are currently supported."

{-# INLINE encodePayload #-}
encodePayload :: Payload -> SerializedPayload
encodePayload = SerializedPayload . S.encode

-- |This class is meant to make it more flexible to use block execution functions.
-- During block execution we only need access to metadata and payload, but in other parts the baker might
-- need other information about a transaction. Instances of this class make it possible to simply project the values
-- without needing to convert transactions first.
class Message a where
  getHeader :: a -> Header
  getPayload :: a -> Either String Payload

-- |A simple type of messages used during testing.
type MessageTy = (Header, SerializedPayload)

instance Message MessageTy where
  {-# INLINE getHeader #-}
  getHeader = fst
  {-# INLINE getPayload #-}
  getPayload = S.decode . _spayload . snd

-- |Events which are generated during transaction execution.
-- These are only used for valid transactions.
data Event = ModuleDeployed !Core.ModuleRef
           | ContractInitialized !Core.ModuleRef !Core.TyName !ContractAddress
           | Updated !ContractAddress !Amount !MessageFormat
           | Transferred !Address !Amount !Address
           | AccountCreated !AccountAddress
  deriving (Show)

-- |Used internally by the scheduler since internal messages are sent as values,
-- and top-level messages are acorn expressions.
data MessageFormat = ValueMessage !Value | ExprMessage !Expr
    deriving(Show)

-- |Result of a valid transaction.
data ValidResult = TxReject InvalidKind | TxSuccess [Event]
    deriving(Show)

-- |Ways a single transaction can fail. Values of this type are only used for reporting of invalid transactions.
data InvalidKind = ModuleNotWF !String -- ^Error raised when typechecking of the module has failed.
                 | ModuleHashAlreadyExists !Core.ModuleRef  -- ^As the name says.
                 | MessageTypeError !String -- ^Message to the receive method is of the wrong type.
                 | ParamsTypeError !String -- ^Parameters of the init method are of the wrong type.
                 | InvalidAccountReference !AccountAddress -- ^Account does not exists.
                 | InvalidContractReference !Core.ModuleRef !Core.TyName -- ^Reference to a non-existing contract.
                 | InvalidModuleReference !Core.ModuleRef   -- ^Reference to a non-existing module.
                 | InvalidContractAddress !ContractAddress -- ^Contract instance does not exist.
                 | EvaluationError !String -- ^Error during evalution.
                                           -- This is mostly for debugging purposes since this kind of an error should not happen after successful typechecking.
                 | AmountTooLarge !Address !Amount
                 -- ^When one wishes to transfer an amount from A to B but there
                 -- are not enough funds on account/contract A to make this
                 -- possible. The data are the from address and the amount to transfer.
                 | SerializationFailure String -- ^Serialization of the body failed for the given reason.
                 | OutOfEnergy -- ^We ran of out energy to process this transaction.
                 | Rejected -- ^Rejected due to contract logic.
                 | AccountAlreadyExists !AccountAddress
                 | AccountCredentialsFailure
    deriving (Show)

data FailureKind = InvalidHeader
                 | InsufficientFunds !AccountAddress   -- ^The amount is not sufficient to cover the gas deposit. 
      deriving(Show)

data TxResult = TxValid ValidResult | TxInvalid FailureKind

-- | This is the part of the global store that is needed by the interpreter to look up, e.g., values of other contract
-- instances, and by the typechecker to know the types of definitions and contract instances.
data GlobalState = GlobalState 
    {instances :: !(HashMap ContractAddress Instance)  -- ^Contract instances.
    ,accounts :: !(HashMap AccountAddress Account)      -- ^FIXME: Unclear what the distinction between accounts/instances is.
    ,modules :: !(HashMap Core.ModuleRef (Interface, ValueInterface)) -- ^Interfaces of deployed modules.
    }

firstFreeContract :: GlobalState -> ContractAddress
firstFreeContract (GlobalState{..}) = 
  ContractAddress (fromIntegral (length instances)) 0 

emptyGlobalState :: GlobalState
emptyGlobalState = GlobalState {
  instances = Map.empty
  , accounts = Map.empty
  , modules = Map.empty
  }
