# Node's data storage

This document describes what data a running node stores, how it is stored, and
how data flows through the system.

## Overview

The node stores transactions, blocks, finalization records, as well as data
derived from executing transactions, e.g., accounts, smart contract modules.
This derived data is stored for each block.

The implementation supports different storage strategies for testing, but this
document will focus on describing the production environment, where data is
stored permanently in different persistent databases.

In this environment the node stores data in two layers. Data pertaining to the
finalized part of the chain is stored in on-disk databases, and some of it is
cached in memory for performance reasons. Data for the non-finalized part of the
chain is only stored in memory. The main effect of this is that on node restarts
the non-finalized part of the chain is lost and needs to be recovered from the
network.

Data is written to databases on each finalization.

### Databases

The node stores data in two internal databases, and an optional external
database. The internal/external distinction refers to how the databases are
intended to be used. Internal means that only the node accesses the database,
whereas external means that the data is written by the node, but read by other
services.

These databases are

1. the **tree state database** which stores
  - finalized blocks indexed by hashes
  - finalization records indexed by finalization index
  - transaction outcomes of finalized transactions (this includes credential
    deployments and updates), indexed by transaction hashes.
  - index of finalized blocks by height.

2. the **block state database** which stores
   - accounts
   - Wasm modules
   - smart contract instances
   - transaction outcomes
   - pending updates
   - auxiliary data such as identity providers, anonymity revokers

   for each block since genesis.

3. **transaction index database** which is optional external database which
   stores an index of transactions by affected account, and by affected smart
   contract. This means that both incoming and outgoing transactions are
   accessible for an account or contract. In contrast to the block and tree
   state this database is never read by the node, only by external services such
   as the wallet-proxy.

The **tree state** is stored in a single LMDB database with 4
**stores** corresponding to the 4 different items that are stored. LMDB is a
simple key-value store, so it only supports lookups by the specified index, and
traversals. It does not support any additional queries.

The **block state** is stored in a simple file in an ad-hoc way defined by the
implementation. Sharing is used so that storage does not grow too quickly. For
example, if an account does not change from one block to another then most of
its data is not copied, only some metadata related to its position in the
account table.

The **transaction index** is stored in an external PostgreSQL database in a
fairly unstructured way. We mostly store transaction outcomes as a JSON blob,
but some metadata is stored in separate columns to enable more efficient
indexing.

### Data processed by the node

When a node runs it sends and receives a number of different message types. In
this document we ignore the low-level network messages, and instead focus on
payloads meaningful to the consensus layer and above. These messages are

- **blocks**, any node receives blocks from and sends them to its peers
- **transactions**, any node receives and sends transactions. Transactions are
  received in three different ways
  - via the gRPC interface
  - from peers
  - as part of a block
- **finalization messages**. Only finalizers create these messages, but other nodes
  receive them, process them, and rebroadcast them to peers. Finalization
  messages are only received from the node's peers.
- **finalization records**. These are only sent directly via catchup requests.
  Finalization records are normally sent and received as part of blocks, as well
  as created by nodes on completed finalizations, based on finalization messages.
- **catchup messages**. These are exchanged between nodes directly, not via
  broadcast to peers. They are used to establish a common tree by exchanging
  information about known blocks and finalization records. Note that these
  messages only contain metadata about the node's trees. If the node's trees
  need to be synchronized then blocks and finalization records are transmitted
  separately.

## The tree state database

The tree state database is an LMDB database. This database is located in the
node's data directory, under `DATABASE_SUB_DIRECTORY_NAME/treestate` where
`DATABASE_SUB_DIRECTORY_NAME` is a compile-time constant. This database has four
stores, all of which are simple key-value stores.

### Block store

- Keys: block hashes
- Values: finalized **stored blocks**. A stored block consists of
  - finalization index, the index of finalization in which this block was
    (directly or indirectly) finalized
  - block metadata
    - block hash
    - block height
    - time when the block was received
    - time when the block was added to the tree
    - number of transactions in the block
    - energy cost of all transactions in the block
    - size of all transactions in the block
    - hash of the last finalized block from the perspective of this block
  - the block itself, in the format that is sent over the network
  - pointer (in the form of an offset in the file) to the **block state
    database** to where the state at the end of the block can be read

### Finalization record store

- Keys: finalization indices
- Values: finalization records

This is used to quickly access finalization records outside of blocks.
Finalization records are part of blocks as well, so there eventually each
finalization record is stored twice, once in the **finalization record store**,
and once in the **block store**.

### Finalized transaction status store

- Keys: transaction hashes
- Values: Information about a finalized transaction consisting of
  - slot number of the block in which the transaction occurred
  - hash of the block in which the transaction occurred
  - position of the transaction in the block

  The actual outcome of the transaction, e.g., whether it succeeded, failed, is
  recorded in the **block state database**.

### Index of finalized blocks by height

- Keys: block height
- Values: block hashes

This is used to quickly look up a block by height. The actual block is, of course,
stored in the **block store**.

## The block state database

This is a single file which stores for each block the state of the chain after
that block derived from activity on it, i.e., from transactions. The main
complexity of this database is that data needs to be shared between different
blocks so that database size does not grow too quickly, under the assumption
that most of the data does not change between blocks.

For each block we store the state of the following items at the end of the
block, i.e., after all the transactions have taken effect.

- accounts table
- smart contract instances
- smart contract modules, both the code that is deployed and processed, ready to
  execute, code
- total amount of GTU and total amount of encrypted GTU, as well as the status
  of reward accounts; the baking reward account, the finalization reward
  account, and the gas account.
- identity providers
- anonymity revokers
- derived birk parameters, this includes the three sets of bakers corresponding
  to the currently active epoch, the next epoch, and the present epoch (where
  baker changes are taking effect), as well as data about the current leadership
  election nonce
- cryptographic parameters (bulletproofs generators, commitment keys, ...).
  These do not currently change between blocks so there is only ever a single
  copy.
- current values of chain parameters and any pending updates
- any active release schedules
- for each transaction in the block its outcome, this includes whether the
  transaction was successful or not, and the events resulting from it were
- list of bakers of blocks in the current epoch. This is used for rewarding the
  bakers.

Each of these items is stored in a bespoke manner, all in the same file, with
internal pointers. Pointers in the database file are offsets from the beginning
of the file, i.e., all references are absolute. Data in this file is never
overwritten, we only ever append to it.

One general consideration in the design of the block state database is that it
supports computing a deterministic hash of the state of the chain at a
particular block. This hash is included in each block that is produced by a
baker, and checked by all nodes that execute it.

Since this hash is, conceptually, the hash of the entire state it would be too
expensive to compute it from scratch for each block. Thus some of the
complexities in the design of the state are to make it possible to update the
hash based on only the parts of the state that changed, and use cached results
for the remaining parts.

### The accounts table

The accounts table stores the state of all accounts. Accounts are addressed by
account addresses, which are arbitrary 32-byte sequences that are user-chosen.
In order to support efficient state hash updates the accounts are stored in a
binary Merkle tree (in particular this means only leaf nodes have data, internal
nodes only have metadata, e.g., hash).

This tree is maintained to be a **left full** binary tree (referred to as LFMB
tree) This is defined to be a tree that has the property that for each node, the
right subtree has height no greater than the left subtree, and only has elements
if the left subtree is full. In order to support deterministic hashing each
account is assigned a sequential index, based on when it was created. This
index, which is a 64-bit number, determines the position in the tree.

As an example, if there are 5 accounts in the table then the tree would look as
follows
```
        *
      /  \
     *    4
   /   \
  *     *
 / \   / \
0  1  2  3
```
where the `*` nodes contain metadata, including the hash of the subtree.

When adding a new account (by necessity with index 5) the tree would become
```
         *
      /    \
     *       *
   /   \    / \
  *     *  4   5
 / \   / \
0  1  2  3
```

This tree has the following properties

- it does not support deletions. Deletions of accounts are not currently
  possible, but if they become necessary the intention is to mark accounts as
  deleted, but not remove the nodes from the tree.
- when adding a new account to a tree of size n only Θ(log₂(n)) hashes need to be recomputed, and only
  this many nodes of the tree need to be updated.
- when updating a single account only Θ(log₂(n)) hashes need to be recomputed,
  and only this many nodes in the tree need to be updated.
- account lookup by index is Θ(log₂(n))

To support account lookups by address we maintain an auxiliary mapping from
account addresses to account indices. This is stored in the form of a trie. This
is only used to make it efficient to look up an account by an address. The trie
is not used when computing the hash of the state, since it can be completely
recovered from the Merkle tree of accounts described above.

To support efficient execution of account creation and credential deployment,
and some queries, we maintain an additional mapping from credential registration
ids to account indices. This is used to check whether a credential registration
id is already used on the chain, as well as to find an account on which a
credential registration id was used to enable queries. This is again stored in
the form of a trie, with an additional cached in-memory version of the same.

### Smart contract instances

A smart contract instance consists of (at a high level)
- metadata, e.g., account that created it, name of the contract
- a reference to the Wasm module
- the current contract state and the contract's current balance

Smart contract instances are addressed via smart contract addresses, which are
pairs of and **index** and **subindex**, both of which are 64-bit integral
values. These addresses, in contrast to account addresses, are assigned by the
protocol, and not chosen by the users. There is no good reason for this other
than there was no specific requirement for it, like there was for accounts, and
not having an additional indirection simplified the implementation.

Smart contract instances are stored in a similar table that account are stored
in, and hashed in an analogous way. The contract **index** determines the
position of the smart contract instance in the table. The **subindex** is a
version of the contract on that index. Because the current protocol does not
allow removal of smart contract instances, the **subindex** is always 0. If
smart contract instance removal was allowed then the idea is that upon removal
of a contract with index **i** and subindex **j**, the space in the table with
index **i** is marked as vacant, with next available subindex **j+1**. When a
new contract instance is created it will be created with the address **(i,
j+1)**.

This design has additional complexity over just marking the contract as deleted
and only using a single index to address contract instances. The advantage it
has over that approach is that the size of the tree grows more slowly in the
case of many smart contract deletions.

### Smart contract modules

Smart contract instances only store references to **modules**. A module contains
- the Wasm module that was sent as part of the transaction
- a **module interface** which is a processed Wasm module stored in a format
  which is more convenient, and more efficient, to execute, as well as
  - the set of exposed contracts in this module
  - for each contract, the set of its receive methods
  - the size of the original Wasm module. This is used for accounting.
  - hash of the original module

Modules are stored in a module table that has a very similar structure to the
accounts table. Modules are referred to via **module references**, which are
defined to be hashes of the Wasm module contents. To support deterministic
hashing, as for accounts, each module is assigned a **module index**, which is a
64-bit integer.

The main storage of modules is in a LFMB tree with keys being module indices.
Modules can never be removed from this tree. As for accounts, we maintain an
additional index mapping module references to module indices, so that we can
look up modules via module references. This is necessary since module indices
are an internal implementation detail, and are not exposed to users of smart
contracts, but module references are.

As for accounts, only the LFMB tree is used in block state hash computation,
since the auxiliary mapping of references to indices can be recovered.

### Bank status

The bank status part of the state records a few tokenomics parameters.
- the total amount of GTU in existence
- the total amount of GTU that is encrypted
- balance of the baking and finalization rewards accounts, as well as the gas
  account.

This is stored in a straighforward way, as a sequence of integers.

### Anonymity revokers and identity providers

For each block we store the identity providers and anonymity revokers. They are
stored in a straightforward way, as a serialized ordered map, mapping identity
provider/anonymity revoker identities to identity provider/anonymity revoker
public keys and metadata.

A consequence of this storage format is that if a new identity
provider/anonymity revoker is added the entire map needs to be copied. This was
deemed acceptable since updates to these collections are very very rare.

Additionally, these collections are hashed by serializating the data and then
computing the hash. There is no Merkle tree structure and incremental hashing.
Again, this is acceptable since updates to these collections, at the moment, are
extremely rare, and can only be made by the foundation.


### Birk parameters

This stores information needed by Consensus for baking. It contains three sets
of bakers, one for the current epoch, one for the previous epoch, and one for two
epochs ago. This latter are the bakers that are currently eligible for producing
blocks.

In addition to these three sets of bakers birk parameters store the current
**seed state**. This contains
- metadata about epochs
- leadership election nonce for the current epoch
- a running hash of blocks in the current epochs that go into computing the
  leadership election nonce for the next epoch.

The seed state is stored as a flat structure, serialized and stored.

Each set of bakers is stored

### Cryptographic parameters

These are stored by serializing the structure. There are currently no updates to
this value, so only a single copy is ever stored, the one given in genesis.

### Chain parameter and protocol updates

This part of the state stores
- current keys authorized to sign the different update types
- whether a protocol update is currently in effect. This is used to stop
  consensus.
- current values of chain parameters, this are reward fractions, inflation rate, etc.
- pending updates

The **pending updates** consist of an update queue for each update type. All
**update queues** follow the same overall structure and differ only in the exact
payload that they store. This payload is dependent on the exact update type.

Each **update queue** consist of the next sequence number for that update, as
well as a list of pairs of effective time and the payload. The payload is stored
in a way that allows sharing. Enqueueing a new update will not copy the payload,
but it will make a copy of the rest of the update queue.

### Release schedule

The release schedule is a result of scheduled transfers. The main complication
is that the effect of the transaction is not only immediate, but the GTU are
release over a period of time. This part of the state keeps the information
needed to efficiently check whether any releases need to be affected at the
beginning of each block. In particular this part just maps account addresses to
the timestamp of the next release. This is used together with the information
stored for each account in the account table, which contains all remaining
releases.

Since this part of the state is uniquely determined from the data stored with
accounts it does not contribute to the state hash computation.

The entire mapping is stored "flat", meaning that any updates to the map cause
the entire map to be written anew. This is alleviated by the fact that the size
of the map is limited by the number of accounts with currently active releases,
and the map is only updated upon an active release.

### Transaction outcomes

For each block we store the outcomes of transactions in that block. The outcome
includes information about the order in the block, whether it succeeded or
failed, the energy and GTU cost of transactions, and any events that resulted
from the transaction. Events are transaction-type specific, and include things
like GTU transfer, and smart contract updates.

Transaction outcomes are currently stored as a "flat" structure, as a serialized
list of transaction outcomes. The outcomes for a block are never updated once
they are created, so this is not too wasteful, the only downside is that it
prevents individual indexing, i.e., looking up the 5th transaction in the block.

This structure does not contribute to the block state hash.

### Epoch blocks

This is list of blocks in the current epoch. It is stored since the information
about how many bakers produced blocks in the epoch is used at the end of the
epoch to compute baking rewards.

# Auxiliary data structures

The following auxiliary data structures are used in block state storage.

## Trie

For storing indices where keys are (equivalent/convertible to) fixed-size byte
arrays we sometimes use tries/prefix trees. We have our own implementation of
tries specialized for our use case. In particular

- branching nodes always have degree 256
- there is an optimization for the case where there is no branching, we directly
  store the path segment that has no branching

The data structure operations never modify the trie in-place, but rather create
a new trie, retaining as many pointers to the old tree as possible.

This trie implementation is generally not used when consistent, deterministic
hashing is needed. The inner nodes do not store any information, so updating of
the hash of the data would generally require recomputing the hash of the entire
tree.

On disk, the tree is stored in a very classical way. Inner nodes write a list of
pointers (i.e., offsets in the block state file) to the subtrees, and the leafs
store the value.

## LFMB tree (Left-Full Merkle Binary tree)

This is used for storing accounts and smart contract modules. A very similar
structure is used for storing smart contract instances, but it is sufficiently
different that it currently uses a different implementation.

As described above, this is a left-full tree that supports efficient
(asymptotically) lookups, insertions, and updates to the hash of the root and
subtrees.

The trees are stored in a straighforward way on when serialized. Leafs store the
value, and inner nodes store pointers to the left and right subtrees (pointers
in the sense of offset from the start of the file).

# Auxiliary tables

In addition to the permanent storage described above the node maintains a number
of auxiliary tables to store temporary data, and indices to support efficient
execution of the consensus and finalization algorithms, and transaction execution.

All of these tables are only kept in-memory, and as a consequence they are lost
if the node restarts.

## Block table

This table maintains the state of all known blocks. For non-finalized and live
blocks it is responsible for storing the block itself, for other blocks it only
maintains metadata. Finalized blocks are stored in the tree store, in the LMDB
database we described above. Dead blocks, i.e., blocks which are either invalid
in some way, or are on a branch behind the last finalized block are marked as
dead, but record of their existence is kept in this table.

## Transaction tables

The transaction table stores, in memory, the non-finalized transactions and two
indices.

The first is an index of non-finalized transactions for an account. This
contains the next nonce (from the perspective of the last finalized block) for
each existing account, and, if there are any, the nonce-indexed transactions
that are not yet finalized for the account. This information is used in two
ways. The next nonce for the account is used to both deduplicate transactions,
both on the network, but also to ensure that no transaction is ever put into two
blocks on the same branch. The index of transactions by nonce is used when the
baker creates blocks. At that time the transactions per account must be ordered
by increasing nonce.

The transaction table is used in combination with the **pending transaction
table**.

The pending transaction table records any transactions that are pending from the
perspective of a **focus block**. The **focus block** is an auxiliary construct
used to maintain the transactions in a reasonably efficient state. In general it
differs from two other special blocks, the **last finalized block** and **best
block**.

While the transaction table maintains the pending transactions themselves, the
pending transaction table maintains metadata needed to efficiently update the
transaction table when we get new blocks, as well as to select transactions from
the transaction table at the time the baker is creating a new block.

The **focus block** is updated by consensus so that it is always a live block,
and never behind the **last finalized block**. When it is a baker's turn to bake
a block the focus block is set to the **best block**, which is a protocol
defined block that the baker is supposed to build upon.

# External transaction index database

The external transaction index database is optionally written to by the node
upon each finalization. It stores transaction outcomes for finalized blocks with
an additional index so that incoming as well as outgoing transactions can be
looked up for an account, as well as for smart contract instances.

The postgres database consists of three tables
- the **summaries** table, which has columns
  - **id** --- the auto-generated id of the summary, i.e., the row number. This
    is the primary key.
  - **block** --- hash of the block the transction is in
  - **timestamp** --- the timestamp, in milliseconds, of the block slot
  - **height** --- the height of the block
  - **summary** --- a JSON value containing the transaction outcome

- the **ati** (account transaction index) table, which has columns
  - **atiId** --- the auto-generated id. This is the primary key.
  - **account** --- the byte-serialized account address, i.e., 32 bytes
  - **summary** --- a foreign key in the **summaries** table.
  The meaning of each row is that the account balance was affected by the
  transaction whose summary is recorded.

- the **cti** (contract transaction index) table which has columns
  - **ctiId** --- the auto-generated id. This is the primary key.
  - **index** and **subindex** --- together they form the contract address
  - **summary** --- a foreign key in the **summaries** table.

Data is written to this database upon each finalization. In addition to account
transaction summaries, the database also contains **special outcomes** which are
protocol defined transfers. They are

- minting
- baking rewards
- block rewards
- finalization rewards

The format of the actual summary is unfortunately an automatically generated
JSON value. The concrete values that are stored are values of the Haskell
datatype `Either TransactionSummary SpecialTransactionOutcome`. The `Either`,
`TransactionSummary`, and `SpecialTransactionOutcome` types all have
automatically derived JSON encodings defined in `concordium-base`. Please see
the type definitions for details of the format.
