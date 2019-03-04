module OakTest.Contract.Spec exposing (Contract)

type Tx = Maybe ()

-- Contracts must define themselves in terms of this ADT so that we know their functions agree on the model type, etc.
-- Currently simplified massively for PoC purposes
type alias Contract model msg =
  { init : model
  , receive : model -> msg -> (model, Tx)
  }


{- Using a `contract :: Contract myModel myMsg`, we'll generate something like this:

  contract :: Contract myModel myMsg
  contract = ...

  exec maybeModelStr maybeMsgStr =
    let msgIn = Maybe.map evg_d_Msg msgStr
        modelIn =
          case maybeModelStr of
            Just modelStr -> evg_d_Model modelStr
            Nothing -> evg_d_Model contract.init
        (modelOut, txOut) = update modelIn msgIn
    in
    -- Our final return is "encoded" model and the tx type (which will be introspected at AST level in Haskell)
    (evg_e_Model modelOut, txOut)

 -}
