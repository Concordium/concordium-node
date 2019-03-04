module IncDec exposing (receive, Msg(..), Model)

import Tx
import Blockchain exposing (Reply(..), Tx(..))

type Msg
  = Increment
  | Decrement


type alias Model =
  { counter : Int
  }


-- We'll integrate this later when we have persistent history and want to simulate "initial state for deploy + subsequent calls"
-- init : () -> Chain -> ( Caller, Amount ) -> Model
-- init flags blockchain ( caller, amount ) =
--     { counter = 0
--     }


-- We'll integrate towards the full signature later as we have more components
-- receive : Chain -> Model -> ( Caller, Amount, Maybe (Msg wrapper) ) -> ( Model, Tx )
-- receive blockchain model ( caller, txAmount, msg ) =


receive : Model -> Maybe Msg -> Reply Model
receive model msg =
  case msg of

    Nothing ->
      Reject

    Just Increment ->
      let
        newModel = { model | counter = model.counter + 1 }
        tx = Transfer "some_addr" 1.0 100
      in
        Accept newModel tx

    Just Decrement ->
      Accept { model | counter = model.counter - 1 } None

