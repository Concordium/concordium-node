module IncDecMain exposing (exec)

import IncDec
import Json.Encode as E
import Json.Decode as D

exec : String -> Maybe String -> (String, Maybe ())
exec modelStr maybeMsgStr =
  let msgIn =
        case maybeMsgStr of
          Just s ->
            case D.decodeString IncDec.evg_d_Msg s of
              Ok msg -> Just msg
              Err _ -> Nothing
          Nothing -> Nothing
      modelIn =
        case D.decodeString IncDec.evg_d_Model modelStr of
          Ok model -> model
          Err _ -> { counter = 0 }
      (modelOut, txOut) = IncDec.receive modelIn msgIn
  in
    (E.encode 0 (IncDec.evg_e_Model modelOut), E.encode 0 (IncDec.evg_e_Tx txOut))
