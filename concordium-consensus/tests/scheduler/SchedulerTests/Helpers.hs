module SchedulerTests.Helpers where

import Concordium.Scheduler.Types
import qualified Concordium.Scheduler.Cost as Cost
import qualified Concordium.Scheduler.Types as Types

getResults :: [(a, TransactionSummary)] -> [(a, ValidResult)]
getResults = map (\(x, r) -> (x, tsResult r))

-- | The cost for processing a simple transfer (account to account)
-- with one signature in the transaction.
-- Payload size estimated to 8 (64bit amount) + 1 (transaction type) (numbers have to be verified).
simpleTransferCost :: Energy
simpleTransferCost = Cost.checkHeader (Types.transactionHeaderSize + 8 + 1) 1
