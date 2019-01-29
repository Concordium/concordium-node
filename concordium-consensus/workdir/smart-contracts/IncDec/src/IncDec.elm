module OakTest.Contract.IncDec exposing (update, Msg(..), Model)

-- We'll bring these in later when we expand the example to include actual Blockchain APIs
-- import Blockchain exposing (..)
-- import Tx


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


{-| We'll replace this with Tx.none later when we expand to include the actual Blockchain.elm APIs
-}
txNone =
  Nothing



-- We'll integrate towards the full signature later as we have more components
-- update : Chain -> Model -> ( Caller, Amount, Maybe (Msg wrapper) ) -> ( Model, Tx )
-- update blockchain model ( caller, txAmount, msg ) =


update model msg =
  case msg of
    Nothing ->
      ( model, {-txNone-} Nothing )

    Just Increment ->
      ( { model | counter = model.counter + 1 }, {-txNone-} Nothing )

    Just Decrement ->
      ( { model | counter = model.counter - 1 }, {-txNone-} Nothing )

exec modelStr maybeMsgStr =
  let msgIn =
        case maybeMsgStr of
          Just "Increment" -> Just Increment
          Just "Decrement" -> Just Decrement
          _ -> Nothing -- we just map errors together with the intentional no message case for now, but that will need to change later
      modelIn =
        case String.toInt modelStr of
          Just x -> { counter = x }
          Nothing -> { counter = 0 } -- bad but fine for now till exec has a wrapper type for decoding errors later
      (modelOut, txOut) = update modelIn msgIn
      serialiseModel m = String.fromInt m.counter
  in
  -- Our final return is "encoded" model and the tx type (which will be introspected at AST level in Haskell)
  (serialiseModel modelOut, txOut)

-- Contract setup - we'll expose/integrate this later when our interpreter type coverage is more comprehensive
--
-- contract : Contract () (Msg wrapper) Model
-- contract =
--     Blockchain.contract version init update
-- version : Version
-- version =


{- Example AST to run:
:{
import qualified AST.Canonical as Can
import qualified AST.Module.Name as ModuleName
import qualified Elm.Package as Pkg
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R
import qualified Data.Map as Map
import qualified Data.Index as Index
import qualified Eval.Env as E
}:
:{
at = A.At $ R.Region (R.Position 0 0) (R.Position 0 0)
model = at $ Can.Record (Map.fromList [("counter", at $ Can.Int 1)])
modu = ModuleName.Canonical (Pkg.Name "author" "project") "OakTest.Contract.IncDec"
incr = at $ Can.VarCtor Can.Normal modu "Increment" (Index.ZeroBased 0) (Can.Forall (Data.Map.empty) (Can.TType modu "Msg" []))
msg = at $ Can.Call (at $ Can.VarCtor Can.Normal ModuleName.maybe "Just" (Index.ZeroBased 0) (Can.Forall (Data.Map.empty) (Can.TType modu "Msg" []))) [incr]
result = Can.Call (at $ Can.VarTopLevel modu "update") [model, msg]
:}
-}
