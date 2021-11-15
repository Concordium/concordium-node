{-# LANGUAGE OverloadedStrings #-}
module SchedulerTests.TransfersWithScheduleTest where

import Test.Hspec


import qualified  Concordium.Crypto.SignatureScheme as Sig

import            Concordium.GlobalState.Basic.BlockState.Accounts as Acc
import            Concordium.GlobalState.Basic.BlockState

import            Concordium.GlobalState.DummyData
import            Concordium.ID.Types as ID
import qualified Data.ByteString.Short as BSS
import Concordium.Types.DummyData
import Concordium.Crypto.DummyData
import            Concordium.Scheduler.DummyData
import qualified  Concordium.Scheduler.Runner as Runner
import            Concordium.Scheduler.Types
import            SchedulerTests.TestUtils

initialBlockState :: BlockState PV1
initialBlockState = blockStateWithAlesAccount
    10000000000
    (Acc.putAccountWithRegIds (mkAccount thomasVK thomasAccount 10000000000) Acc.emptyAccounts)

initialBlockState2 :: BlockState PV2
initialBlockState2 = blockStateWithAlesAccount
    10000000000
    (Acc.putAccountWithRegIds (mkAccount thomasVK thomasAccount 10000000000) Acc.emptyAccounts)

initialBlockState3 :: BlockState PV3
initialBlockState3 = blockStateWithAlesAccount
    10000000000
    (Acc.putAccountWithRegIds (mkAccount thomasVK thomasAccount 10000000000) Acc.emptyAccounts)

cdiKeys :: CredentialDeploymentInformation -> CredentialPublicKeys
cdiKeys cdi = credPubKeys (NormalACWP cdi)

vk :: Sig.KeyPair -> Sig.VerifyKey
vk = Sig.correspondingVerifyKey


testCases1 :: [TestCase PV1]
testCases1 =
  [ TestCase
    { tcName = "Transfers with schedule"
    , tcParameters = defaultParams {tpInitialBlockState=initialBlockState, tpChainMeta=ChainMetadata { slotTime = 100 }}
    , tcTransactions = [
        -- trying to do a scheduled transfer with memo - should get rejected since we have protocol version 1
        ( Runner.TJSON  { payload = Runner.TransferWithScheduleAndMemo thomasAccount (Memo $ BSS.pack [0,1,2,3]) [(101, 10),(102, 11),(103, 12)],
                          metadata = makeDummyHeader alesAccount 1 100000,
                          keys = [(0,[(0, alesKP)])]
                        }
        , (Reject SerializationFailure
          , emptySpec
          )
        ),
        -- make a scheduled transfer
        ( Runner.TJSON  { payload = Runner.TransferWithSchedule thomasAccount [(101, 10),(102, 11),(103, 12)],
                          metadata = makeDummyHeader alesAccount 2 100000,
                          keys = [(0,[(0, alesKP)])]
                        }
        , (SuccessE [TransferredWithSchedule alesAccount thomasAccount [(101, 10),(102, 11),(103, 12)]]
          , emptySpec
          )
        ),
        -- should get rejected since timestamps are not increasing
        ( Runner.TJSON  { payload = Runner.TransferWithSchedule thomasAccount [(101, 10),(103, 11),(102, 12)],
                          metadata = makeDummyHeader alesAccount 3 100000,
                          keys = [(0,[(0, alesKP)])]
                        }
        , (Reject NonIncreasingSchedule
          , emptySpec
          )
        ),
        -- should get rejected since first timestamp has expired
        ( Runner.TJSON  { payload = Runner.TransferWithSchedule thomasAccount [(99, 10),(102, 11),(103, 12)],
                          metadata = makeDummyHeader alesAccount 4 100000,
                          keys = [(0,[(0, alesKP)])]
                        }
        , (Reject FirstScheduledReleaseExpired
          , emptySpec
          )
        ),
        -- should get rejected since sender = receiver
        ( Runner.TJSON  { payload = Runner.TransferWithSchedule alesAccount [(101, 10),(102, 11),(103, 12)],
                          metadata = makeDummyHeader alesAccount 5 100000,
                          keys = [(0,[(0, alesKP)])]
                        }
        , (Reject $ ScheduledSelfTransfer alesAccount
          , emptySpec
          )
        ),
        -- should get rejected since one of the amounts is 0
        ( Runner.TJSON  { payload = Runner.TransferWithSchedule thomasAccount [(101, 10),(102, 0),(103, 12)],
                          metadata = makeDummyHeader alesAccount 6 100000,
                          keys = [(0,[(0, alesKP)])]
                        }
        , (Reject ZeroScheduledAmount
          , emptySpec
          )
        )
      ]
    }
  ]

testCases2 :: [TestCase PV2]
testCases2 =
  [ TestCase
    { tcName = "Transfers with schedule and memo"
    , tcParameters = defaultParams {tpInitialBlockState=initialBlockState2, tpChainMeta=ChainMetadata { slotTime = 100 }}
    , tcTransactions = [
        -- make a scheduled transfer with memo - should succeed since protocol version is 2
        ( Runner.TJSON  { payload = Runner.TransferWithScheduleAndMemo thomasAccount (Memo $ BSS.pack [0,1,2,3]) [(101, 10),(102, 11),(103, 12)],
                          metadata = makeDummyHeader alesAccount 1 100000,
                          keys = [(0,[(0, alesKP)])]
                        }
        , (SuccessE [TransferredWithSchedule alesAccount thomasAccount [(101, 10),(102, 11),(103, 12)], TransferMemo (Memo $ BSS.pack [0,1,2,3])]
          , emptySpec
          )
        ),
        -- make a scheduled transfer
        ( Runner.TJSON  { payload = Runner.TransferWithSchedule thomasAccount [(101, 10),(102, 11),(103, 12)],
                          metadata = makeDummyHeader alesAccount 2 100000,
                          keys = [(0,[(0, alesKP)])]
                        }
        , (SuccessE [TransferredWithSchedule alesAccount thomasAccount [(101, 10),(102, 11),(103, 12)]]
          , emptySpec
          )
        ),
        -- should get rejected since timestamps are not increasing
        ( Runner.TJSON  { payload = Runner.TransferWithSchedule thomasAccount [(101, 10),(103, 11),(102, 12)],
                          metadata = makeDummyHeader alesAccount 3 100000,
                          keys = [(0,[(0, alesKP)])]
                        }
        , (Reject NonIncreasingSchedule
          , emptySpec
          )
        ),
        -- should get rejected since first timestamp has expired
        ( Runner.TJSON  { payload = Runner.TransferWithSchedule thomasAccount [(99, 10),(102, 11),(103, 12)],
                          metadata = makeDummyHeader alesAccount 4 100000,
                          keys = [(0,[(0, alesKP)])]
                        }
        , (Reject FirstScheduledReleaseExpired
          , emptySpec
          )
        ),
        -- should get rejected since sender = receiver
        ( Runner.TJSON  { payload = Runner.TransferWithSchedule alesAccount [(101, 10),(102, 11),(103, 12)],
                          metadata = makeDummyHeader alesAccount 5 100000,
                          keys = [(0,[(0, alesKP)])]
                        }
        , (Reject $ ScheduledSelfTransfer alesAccount
          , emptySpec
          )
        ),
        -- should get rejected since one of the amounts is 0
        ( Runner.TJSON  { payload = Runner.TransferWithSchedule thomasAccount [(101, 10),(102, 0),(103, 12)],
                          metadata = makeDummyHeader alesAccount 6 100000,
                          keys = [(0,[(0, alesKP)])]
                        }
        , (Reject ZeroScheduledAmount
          , emptySpec
          )
        ),
        -- should get rejected since timestamps are not increasing
        ( Runner.TJSON  { payload = Runner.TransferWithScheduleAndMemo thomasAccount (Memo $ BSS.pack [0,1,2,3]) [(101, 10),(103, 11),(102, 12)],
                          metadata = makeDummyHeader alesAccount 7 100000,
                          keys = [(0,[(0, alesKP)])]
                        }
        , (Reject NonIncreasingSchedule
          , emptySpec
          )
        ),
        -- should get rejected since first timestamp has expired
        ( Runner.TJSON  { payload = Runner.TransferWithScheduleAndMemo thomasAccount (Memo $ BSS.pack [0,1,2,3]) [(99, 10),(102, 11),(103, 12)],
                          metadata = makeDummyHeader alesAccount 8 100000,
                          keys = [(0,[(0, alesKP)])]
                        }
        , (Reject FirstScheduledReleaseExpired
          , emptySpec
          )
        ),
        -- should get rejected since sender = receiver
        ( Runner.TJSON  { payload = Runner.TransferWithScheduleAndMemo alesAccount (Memo $ BSS.pack [0,1,2,3]) [(101, 10),(102, 11),(103, 12)],
                          metadata = makeDummyHeader alesAccount 9 100000,
                          keys = [(0,[(0, alesKP)])]
                        }
        , (Reject $ ScheduledSelfTransfer alesAccount
          , emptySpec
          )
        ),
        -- should get rejected since one of the amounts is 0
        ( Runner.TJSON  { payload = Runner.TransferWithScheduleAndMemo thomasAccount (Memo $ BSS.pack [0,1,2,3]) [(101, 10),(102, 0),(103, 12)],
                          metadata = makeDummyHeader alesAccount 10 100000,
                          keys = [(0,[(0, alesKP)])]
                        }
        , (Reject ZeroScheduledAmount
          , emptySpec
          )
        )
      ]
    }
  ]  

testCases3 :: [TestCase PV3]
testCases3 =
  [ TestCase
    { tcName = "Transfers with schedule P3"
    , tcParameters = defaultParams {tpInitialBlockState=initialBlockState3, tpChainMeta=ChainMetadata { slotTime = 100 }}
    , tcTransactions = [
        -- should get rejected since sender = receiver, even if addresses are not exactly the same, but refer to the same account.
        ( Runner.TJSON  { payload = Runner.TransferWithSchedule (createAlias alesAccount 2) [(101, 10),(102, 11),(103, 12)],
                          metadata = makeDummyHeader (createAlias alesAccount 1) 1 100000,
                          keys = [(0,[(0, alesKP)])]
                        }
        , (Reject $ ScheduledSelfTransfer alesAccount -- the rejection reason is meant to have the canonical address.
          , emptySpec
          )
        )
      ]
    }
  ]



tests :: Spec
tests = do 
  describe "TransfersWithSchedule" $ mkSpecs testCases1 
  describe "TransfersWithScheduleAndMemo" $ mkSpecs testCases2
  describe "TransfersWithScheduleP3" $ mkSpecs testCases3
