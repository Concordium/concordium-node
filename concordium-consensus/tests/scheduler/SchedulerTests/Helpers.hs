module SchedulerTests.Helpers where

import Concordium.Scheduler.Types
import qualified Concordium.Scheduler.Cost as Cost
import qualified Concordium.Scheduler.Types as Types

getResults :: [(a, TransactionSummary)] -> [(a, ValidResult)]
getResults = map (\(x, r) -> (x, tsResult r))

-- | The cost for processing a simple transfer (account to account)
-- with one signature in the transaction.
--
-- * @SPEC: <$DOCS/Transactions#transaction-cost-header-simple-transfer>
simpleTransferCost :: Energy
simpleTransferCost = Cost.checkHeader (Types.transactionHeaderSize + 42) 1

-- |Protocol version
type PV = 'Types.P0
