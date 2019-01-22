module Concordium.Show where

import Data.ByteString.Lazy.Char8 (unpack)
import Data.ByteString.Builder
import qualified Data.ByteString as SBS


import Concordium.Payload.Transaction
import Concordium.Types

showBSHex :: SBS.ByteString -> String
showBSHex bs = unpack (toLazyByteString $ byteStringHex bs)

showsBlock :: Block -> ShowS
showsBlock block rest = show bh ++
        "[slot=" ++ show (blockSlot block) ++
        "; pointer=" ++ show (blockPointer block) ++
        "; baker=" ++ show (blockBaker block) ++
        "]\n" ++ foldr (\tr -> shows tr . ('\n':)) rest trs
    where
        bh = hashBlock block
        trs = if blockSlot block == 0 then [] else 
            case toTransactions (blockData block) of
                Nothing -> []
                Just l -> l