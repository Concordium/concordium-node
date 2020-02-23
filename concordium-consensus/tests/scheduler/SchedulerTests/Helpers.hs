module SchedulerTests.Helpers where

import Concordium.Scheduler.Types

getResults :: [(a, TransactionSummary)] -> [(a, ValidResult)]
getResults = map (\(x, r) -> (x, tsResult r))
