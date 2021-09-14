This document describes format of the response of state queries, as well as the
meaning of the fields.

# GetConsensusStatus : `ConsensusStatus`

Returns a JSON object with fields describing the current state of the consensus layer.
These fields are broken down into four categories.

### General
These fields are updated as they change, which is typically as a result of a block arriving (being validated) or a finalization record being validated.

* `bestBlock : BlockHash` &mdash; hash of the current best block in the tree
* `genesisBlock : BlockHash` &mdash; hash of the genesis block (never changes)
* `genesisTime : UTCTime` &mdash; genesis time as specified in the genesis block
  (never changes for a given network)
* `lastFinalizedBlock : BlockHash` &mdash; hash of the last finalized block
* `bestBlockHeight : Int` &mdash; height of the best block
* `lastFinalizedBlockHeight : Int` &mdash; height of the last finalized block
* `protocolVersion : Int` &mdash; the currently active protocol version. This
  determines supported transactions, their behaviour, as well as general
  behaviour of consensus. Currently protocol versions 1 and 2 are possible.
* `currentEraGenesisBlock : BlockHash` &mdash; hash of the genesis block after
  the latest protocol update.
* `currentEraGenesisTime : UTCTime` &mdash; slot time of the genesis block for
  the current era.
* `slotDuration : Int` &mdash; duration in milliseconds of a slot. This only
  changes on protocol updates.
* `genesisIndex : Int` &mdash; the number of protocol updates that have taken effect.

### Received block statistics
These statistics are updated whenever a block is received from the network.
A received block is not immediately added to the tree; first it must be validated, which takes some time, and may require data (such as other blocks) that are not available at the time it is received.
When graphing these statistics against time, the time point should be taken as `blockLastReceivedTime`.

* `blocksReceivedCount : Int` &mdash; the number of blocks that have been received from peers
* `blockLastReceivedTime : ?UTCTime` &mdash; the time that a block was last received
* `blockReceiveLatencyEMA : Seconds` &mdash; exponential moving average of block receive latency (in seconds), i.e. the time between a block's nominal slot time, and the time at which is received
* `blockReceiveLatencyEMSD : Seconds` &mdash; exponential moving standard deviation of block receive latency
* `blockReceivePeriodEMA : ?Seconds` &mdash; exponential moving average of the time between receiving blocks (in seconds)
* `blockReceivePeriodEMSD : ?Seconds` &mdash; exponential moving standard deviation of time between receiving blocks

Exponential moving averages are computed with weight 0.1 (that is, the latest value is weighted 1:9 with the previous average).
When rendering these EMAs as a graph, the standard deviations can be rendered as error bars; they give an idea of how variable the statistic is.

### Verified block statistics

These statistics are updated whenever a block arrives (that is, verified and added to the block tree).
When graphing these statistics against time, the time point should be taken as `blockLastArrivedTime`.

* `blocksVerifiedCount : Int` &mdash; the number of blocks that have arrived.
  Note that blocks produced by the node itself are counted in this statistic, but not in `blocksReceivedCount`.
* `blockLastArrivedTime : ?UTCTime` &mdash; the time that a block last arrived
* `blockArriveLatencyEMA : Seconds`, `blockArriveLatencyEMSD : Seconds` &mdash; the exponential moving average and standard deviation of the time between a block's nominal slot time, and the time at which it is verified
* `blockArrivePeriodEMA : ?Seconds`, `blockArrivePeriodEMSD : ?Seconds` &mdash; the EMA/EMSD of the time between blocks being verified
* `transactionsPerBlockEMA : Number`, `transactionsPerBlockEMSD : Number` &mdash; the EMA/EMSD of number of transactions per block

### Finalization statistics

These statistics are updated whenever a block is finalized.
When graphing these statistics against time, the time point should be taken as `lastFinalizedTime`.

* `finalizationCount : Int` &mdash; the number of finalization records that have been validated; this will be less than the `lastFinalizedBlockHeight`, since a finalization record can finalize multiple blocks
* `lastFinalizedTime : ?UTCTime` &mdash; time at which a block last became
  finalized (will be `null` until the node observes a finalization)
* `finalizationPeriodEMA : ?Seconds`, `finalizationPeriodEMSD : ?Seconds`
  &mdash; the EMA/EMSD of the time between finalizations (will be `null` until a node observes finalizations)

# GetBlockInfo : `BlockHash -> ?BlockInfo`

Given a block hash, returns a JSON object with various details about a particular block.

`BlockInfo` object:

* `blockHash : BlockHash` &mdash; the hash of the block (base 16 encoded)
* `blockParent : BlockHash` &mdash; the hash of the parent block
* `blockLastFinalized : BlockHash` &mdash; the hash of the last block that was finalized when this block was created
* `blockHeight : Int` &mdash; height of this block in the tree from genesis
* `eraBlockHeight : Int` &mdash; height of the block since the last protocol update
* `genesisIndex : Int` &mdash; the genesis index (i.e., how many protocol updates have taken effect) of the era the block is in
* `blockReceiveTime : UTCTime` &mdash; time at which the block was received (subjective)
* `blockArriveTime : UTCTime` &mdash; time at which the block was validated (subjective)
* `blockSlot : Int` &mdash; slot number of this block. Note that the slot
  numbers reset on protocol updates.
* `blockSlotTime : UTCTime` &mdash; time at which this block was nominally baked
* `blockBaker : ?Int` &mdash; identity (index) of the baker of this block. Will
  be `null` exactly for genesis and regenesis blocks.
* `finalized : Bool` &mdash; boolean indicating if this block has been finalized yet.
* `transactionCount : Int` &mdash; the number of transactions in this block
* `transactionEnergyCost : Int` &mdash; The amount of NRG used to execute this block.

Note that for the genesis block, the `blockParent` and `blockLastFinalized` will have the hash of the genesis block itself, and the `blockBaker` will be `null`.
`blockReceiveTime` and `blockArriveTime` are subjective, and will vary between nodes.
A block that becomes finalized will have its `finalized` field set to `true` &mdash; this will only change once.
A block that is dead or never seen will return the value `null`.

# GetAncestors : `BlockHash -> ?[BlockHash]`

Given a block hash and a number, returns the a list of the given block hash and the hashes of its ancestors going back the given number of generations.
The length of the list will be the given number, or the list will be the entire chain going back from the given block until the closest genesis or regenesis block.
If the block is not live or finalized, the function returns `null`. If the input is not a valid hex string, an error message "Invalid block hash" will be returned.

# GetBranches : `Branch`

Returns a JSON object representing the branches of the tree from the last finalized block.

`Branch` object:

* `blockHash : BlockHash` is the hash of the block
* `children : [Branch]` is a list of JSON objects encoding the children of the block, similarly encoded

# Types

| Type | Representation |
| ---- | -------------- |
| `AccountAddress` | base-58 check with version byte 1 encoded address (with Bitcoin mapping table) |
| `ContractAddress` | is a JSON record with two fields {`index : Int`, `subindex : Int`} |
| `BlockHash` | base-16 encoded hash of a block (64 characters) |
| `TransactionHash` | base-16 encoded hash of a transaction (64 characters) |
| `ModuleRef` | base-16 encoded module reference (64 characters) |
| `Int` | integer | 
| `EncryptionKey` | base-16 encoded string (192 characters) |
| `EncryptedAmount` | base-16 encoded string (384 characters) |
| `UTCTime` | UTC time. `yyyy-mm-ddThh:mm:ss[.sss]Z` (ISO 8601:2004(E) sec. 4.3.2 extended format) |
| `Seconds` | Decimal number of seconds |
| `Number` | Decimal number |
| `?a` | either type `a` or `null` |
| `[a]` | list of values of type `a` |

# Block state queries

The following block state queries are supported. They all return the relevant
items in the specified block, if they exist.

## GetAccountList : `BlockHash -> ?[AccountAddress]`

Get a list of accounts that exist in the given block. `null` indicates the block
does not exist.

## GetInstances : `BlockHash -> ?[ContractAddress]`

Get a list of smart contract instances that exist in the given block. `null`
indicates taht the block does not exist.

## GetAccountInfo : `BlockHash -> AccountAddress -> ?AccountInfo`

Get the state of an account in the given block.

`AccountInfo` is a JSON record with the following fields
- `accountNonce : Int` &mdash; next available nonce on this account at this block
- `accountAmount : Int` &mdash; current public account balance
- `accountEncryptionKey : EncryptionKey` &mdash; an Elgamal public key used to send encrypted transfers to the account.
- `accountIndex : Int` &mdash; sequential index of the account (in the order of creation). If the account is a baker this index is used as the `bakerId`.
- `accountThreshold : Int` &mdash; a non-zero positive integer that specifies how many credentials must sign any transaction from the account.
- `accountCredentials : [CredentialValue]` &mdash; account credential values deployed on the account
- `accountEncryptedAmount : EncryptedAmount` &mdash; the current encrypted
  balance of the account. This is an object with fields
  - `incomingAmounts : [EncryptedAmount]` &mdash; amounts that were sent to this account
    by other accounts. The list has size at most 32.
  - `selfAmount : EncryptedAmount` &mdash; the encrypted amount that is the result of
    actions of the account, i.e., shielding, unshielding, and sending encrypted
    transfers.
  - `startIndex : Int` &mdash; the starting index for amounts in
    `incomingAmounts`. This is needed when sending encrypted transfers to
    indicate which amounts from the list of `incomingAmounts` have been used as
    input to the encrypted transfer.
- `accountCredentials : AccountCredentials` &mdash; an object with keys
  "credential indices" and values versioned credential values. Credential indices are
  integers from 0 up to (at most) 255. The object always has a key `0`.
- `accountBaker : ?AccountBaker` &mdash; if the account is registered as a baker
  this is present and is an object with the following fields
  - `restakeEarnings : bool` &mdash; whether earnings are added to the baker
    stake or not
  - `bakerId : Int` &mdash; the same as `accountIndex`. Id used to identify the
    baker.
  - `bakerAggregationVerifyKey : AggregationVerifyKey` &mdash; hex encoded
    public key used to check signatures on some finalization messages and on
    finalization records (192 characters)
  - `bakerSignatureVerifyKey : SignatureVerifyKey` &mdash; hex encoded
    public key used to check block signatures (64 characters)
  - `bakerElectionVerifyKey : ElectionVerifyKey` &mdash; hex encoded
    public key used to check the proof that the baker won the lottery (192 characters)

## GetInstanceInfo : `BlockHash -> ContractAddress -> ?ContractInfo`

Get the information about the specific smart contract instance.

`ContractInfo` is a record with the following fields

- `amount : Amount` &mdash; the amount of GTU the contract owns.
- `sourceModule : ModuleRef` &mdash; source module for the code of the smart contract
- `owner : AccountAddress` &mdash; address of the account that created the smart
    contract
- `methods : [String]` &mdash; list of methods that the smart contract supports
- `name : String` &mdash; name of the smart contract
- `model : ByteString` &mdash; hex-encoded state of the smart contract

## GetRewardStatus `BlockHash -> ?RewardStatus`

Get an overview of the current balance of special accounts.

`RewardStatus` is a record with fields
- `totalAmount : Amount` &mdash; the total amount of currency in existence at the end of this block
- `totalEncryptedAmount : Amount` &mdash; total amount of encrypted amounts in existence at the end of the block
- `gasAccount : Amount` &mdash; the balance of the GAS account
- `bakingRewardAccount : Amount` &mdash; the balance of the baking reward account
- `finalizationRewardAccount : Amount` &mdash; the balance of the finalization reward account


## GetBirkParameters : `BlockHash -> ?BirkParameters`

Get an overview of the parameters used for baking.

`BirkParameters` is a record with fields
- `electionDifficulty : Number` &mdash; election difficulty for block election
- `electionNonce : String` &mdash; base-16 encoded leadership election nonce.
- `bakers : [BakerInfo]` where `BakerInfo` is a JSON record with the following fields
   - `bakerId : Int` &mdash; unique id of the baker
   - `bakerAccount : AccountAddress` &mdash; address of the account to which the baker gets their reward
   - `bakerLotteryPower : Number` &mdash; the baker's current lottery power. At the moment this is still fixed at genesis, but in the future the lottery power will be calculated based on their stake.

## GetModuleList : `BlockHash -> ?[ModuleRef]`

Get the list of smart contract modules. Returns `null` if the required block
does not exist.

## GetModuleSource : `BlockHash -> ModuleRef -> ?ModuleSource`

Get the source of the module as it was deployed on the chain. The response is a
byte array.
