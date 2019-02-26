module Reply exposing (accept, reject)

import Blockchain exposing (Reply(..), Tx)
import Tx


reject : Reply state
reject =
    Reject


accept : state -> Tx -> Reply state
accept =
    Accept

---- Needed for `callContract` integration into the end-to-end PoC
mapState : (state -> newState) -> Reply state -> Reply newState
mapState f reply =
  case reply of
    Accept state tx -> Accept (f state) tx
    Reject -> Reject
