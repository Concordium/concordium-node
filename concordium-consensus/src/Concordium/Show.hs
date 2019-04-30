module Concordium.Show where

import Data.ByteString.Lazy.Char8 (unpack)
import Data.ByteString.Builder
import qualified Data.ByteString as SBS


import Concordium.GlobalState.Block
import Concordium.Types.HashableTo
import Concordium.Types

showBSHex :: SBS.ByteString -> String
showBSHex bs = unpack (toLazyByteString $ byteStringHex bs)

showsBlock :: Block -> ShowS
showsBlock block rest = show bh ++ 
        case blockFields block of
            Nothing -> "[GENESIS]"
            Just bf -> "[slot=" ++ show (blockSlot block) ++
                        "; pointer=" ++ show (blockPointer bf) ++
                        "; baker=" ++ show (blockBaker bf) ++
                        "]\n" ++ rest
    where
        bh = getHash block :: BlockHash
