module GlobalStateTests.AccountReleaseScheduleMigration where

import Data.Function
import Lens.Micro.Platform
import Test.HUnit
import Test.Hspec

import Concordium.Types

import qualified Concordium.GlobalState.Basic.BlockState.AccountReleaseScheduleV0 as TARSV0
import qualified Concordium.GlobalState.Basic.BlockState.AccountReleaseScheduleV1 as TARSV1

th1, th2, th3 :: TransactionHash
th1 = read "0000000000000000000000000000000000000000000000000000000000000001"
th2 = read "0000000000000000000000000000000000000000000000000000000000000002"
th3 = read "0000000000000000000000000000000000000000000000000000000000000003"

dummyTARSV0 :: TARSV0.AccountReleaseSchedule
dummyTARSV0 =
    TARSV0.emptyAccountReleaseSchedule
        & TARSV0.addReleases ([(10, 10), (20, 10), (30, 10), (40, 10)], th1)
        & TARSV0.addReleases ([(5, 10), (10, 10), (15, 10), (20, 10)], th2)
        & TARSV0.addReleases ([(25, 10), (30, 10), (35, 10), (40, 10)], th3)
        & view _3 . TARSV0.unlockAmountsUntil 20

dummyTARSV1 :: TARSV1.AccountReleaseSchedule
dummyTARSV1 =
    TARSV1.emptyAccountReleaseSchedule
        & TARSV1.addReleases ([(10, 10), (20, 10), (30, 10), (40, 10)], th1)
        & TARSV1.addReleases ([(5, 10), (10, 10), (15, 10), (20, 10)], th2)
        & TARSV1.addReleases ([(25, 10), (30, 10), (35, 10), (40, 10)], th3)
        & view _3 . TARSV1.unlockAmountsUntil 20

tests :: Spec
tests = describe "GlobalState.AccountReleaseScheduleMigration" $ do
    specify "Transient: schedule after migration is as expected" $
        assertEqual
            "V1 summary and migrated V0 summary"
            dummyTARSV1
            (TARSV1.fromAccountReleaseScheduleV0 dummyTARSV0)