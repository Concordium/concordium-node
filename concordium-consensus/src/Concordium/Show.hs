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
showsBlock block rest = show bh ++ if blockSlot block == 0 then "[GENESIS]" else 
        "[slot=" ++ show (blockSlot block) ++
        "; pointer=" ++ show (blockPointer block) ++
        "; baker=" ++ show (blockBaker block) ++
        "]\n" ++ rest -- ++ foldr (\tr -> shows tr . ('\n':)) rest trs
    where
        bh = getHash block :: BlockHash
        -- trs = unhashed <$> blockTransactions block
