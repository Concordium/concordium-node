module Tx exposing (Msg(..), batch, contractCall, none, send, transfer)

import Blockchain exposing (Address, Amount, MaxAllowedGasCost, Reply(..), Tx(..))
import Debug exposing (toString)
import Set exposing (Set)


none : Tx
none =
    None


send : Address -> msg -> Amount -> Tx
send address value amount =
    -- @TODO replace toString with Wire
    Send address (toString value) amount 0


{-| Special send when contract is willing to add some of their own Gas to help ensure Caller's transaction succeeds.

Potential usecases might be;

  - You are the owner of the contract, so the contract checks for your address specifically and adds Gas to it
  - Your bank has a contract that pays to create specific contracts for callers with credentials signed by them

Overview of stuff to remember to make better docs for later:

  - Caller's Gas is always used first
  - Any additional Gas specified by callee is then used
  - When total Gas runs out, transaction is invalid
  - If contract does not have the added amount in balance it's not added

-}
sendWithOwnGas : Address -> msg -> Amount -> MaxAllowedGasCost -> Tx
sendWithOwnGas address msg amount gas =
    -- @TODO replace toString with Wire
    Send address (toString msg) amount gas


batch : Set Tx -> Tx
batch transactions =
    Batch transactions


{-| send money from the current contract to someone else
-}
transfer : Address -> Amount -> MaxAllowedGasCost -> Tx
transfer address amount maxGasCost =
    Transfer address amount maxGasCost



-- NOTE: it's always a fixed cost to send money to an account. If that account sends messages as a result, e.g. to a contract, the owner of that account pays the gas.
-- wrap result from other data types so we can wrap that into our own msg type


contractCall : Address -> Amount -> MaxAllowedGasCost -> Msg toMsgB -> Tx
contractCall address amount maxGasCost msg =
    -- @TODO STUB
    none



-- We can't paramaterise `Msg (b -> msgB)` so...


type Msg toMsgB
    = Msg



-- txa =
--   contractCall "abc" 3.14 (\x -> BuyReceived x)
