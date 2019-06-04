{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Concordium.GlobalState.Rewards where

import Concordium.Types
import Concordium.ID.Types

import Data.HashMap.Strict(HashMap)
import qualified Data.HashMap.Strict as HM

import Lens.Micro.Platform

-- |This datastructure contains the data necessary to reward all the parties on
-- the chain after each block. It also contains the total amount of GTU in
-- existence so that inflation (minting) can be done. This data is meant to be
-- from the point of view of the end of the block (same as other data in
-- BlockState).
-- 
-- TODO:
--
--  * Identites of accounts which have deployed the code of smart contracts
--    which was involved in contract execution.
data BankStatus = BankStatus {
  -- |Total amount of GTU in existence.
  _totalGTU :: Amount,
  -- |Total amount of encrypted amounts. This is not an important field, but can
  -- be used for debugging.
  _totalEncryptedGTU :: Amount,
  -- |The amount of GTU currently in the central bank. Can be used to reward
  -- bakers and other parties.
  _centralBankGTU :: Amount,
  -- |Identity issuers involved in transactions in the current block with the
  -- number of transactions they were involved in. Their rewards will be
  -- calculated based on that. Note that their reward does not depend on any
  -- parameters of the transaction (such as total gas cost), but only that they
  -- were involved in a transaction.
  _identityIssuersRewards :: HashMap IdentityProviderPublicKey Word,
  -- |Rewards to go to the finalization committee if and once this block is
  -- finalized. We propose the following mechanism. The finalization committee
  -- gets rewarded the first time another block's last finalized pointer points
  -- to the current block. The amounts they get rewarded with accrue with each
  -- non-finalized block. There might be other incentive mechanisms. For
  -- instance, with each non-finalized block there could be a smaller fraction
  -- of the total block reward rewarded to the finalization committe since they
  -- are not doing their job.
  _finalizationReward :: Amount,
  -- |Total execution cost for all transactions on this block, in GTU according
  -- to the exchange rate determined for this block.
  _executionCost :: Amount,
  -- |Inflation rate for all slots from the current block's one until the next
  -- block's. The value is the amount of GTU created per slot. Since slot time
  -- is fixed at genesis time this value, together with the initial amount of
  -- GTU, this value can be adjusted to correspond to the desired inflation
  -- rate.
  _mintedGTUPerSlot :: Amount
  } deriving(Show)

makeLenses ''BankStatus

-- |Bank with no money.
emptyBankStatus :: BankStatus
emptyBankStatus = BankStatus{..}
  where _totalGTU = 0
        _totalEncryptedGTU = 0
        _centralBankGTU = 0
        _identityIssuersRewards = HM.empty
        _finalizationReward = 0
        _executionCost = 0
        _mintedGTUPerSlot = 10


makeGenesisBankStatus :: Amount -- ^Total amount of GTU in existence.
                      -> Amount -- ^Amount of minted GTU per slot
                      -> BankStatus
makeGenesisBankStatus _totalGTU _mintedGTUPerSlot = BankStatus{..}
  where _totalEncryptedGTU = 0
        _centralBankGTU = 0
        _identityIssuersRewards = HM.empty
        _finalizationReward = 0
        _executionCost = 0
