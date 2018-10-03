module Concordium.Payload.Check where

import Concordium.Types
import Concordium.Skov.Monad

-- |Check if the data in a block is valid (in the context of its chain).
-- Currently, no validation is performed.
checkData :: (SkovMonad m) => Block -> m Bool
checkData block = return True