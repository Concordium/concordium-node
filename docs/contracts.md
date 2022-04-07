# Smart contracts

This document describes the smart contracts from the perspective of the node and
the consensus protocol.

Smart contracts are based on WebAssembly, specifically on the [first standardised
version of Wasm](https://www.w3.org/TR/wasm-core-1/) with additional
restrictions. These restrictions are
- all floating point instructions and types are disallowed
- initial memory size is limited to 32 pages
- maximum number of locals allowed in functions is 1024
- maximum stack height is 1024 (this means locals + dynamic stack height)
- the number of globals is limited to 1024
- the maximum number of cases in a switch statement is limited to 4096
- the list of imports is restricted to allowed ones
- the list of exports is restricted, names and types are restricted
  (restrictions differ between V0 and V1 versions, see below)
- Wasm modules cannot have an initialization function.
- The size of the module is restricted. The limit is different for V0 and V1 modules.

Smart contracts interact with the chain through the exposed **host functions**. This
is a general Wasm concept that allows controlled sandboxing. The host functions
that are allowed are listed for V0 and V1 contracts below.

There are three operations related to smart contracts
- **deploying** a Wasm module. This registers a new Wasm module on the chain. The
  module is validated and processed into an "Artifact" which is ready to
  execute. This processing also adds resource accounting, see the [NRG](#nrg) section
  below.
- **initializing** a smart contract instance. Given a deployed module and a name of
  the contract construct a new contract instance. This has its own state and
  balance.
- **updating** an existing smart contract instance. This can affect other smart
  contract instances by sending messages, or operation invocation, depending on
  the version of the instance.

These operations are triggered by transactions, and orchestrated by the
scheduler component of the node. The execution of these operations, e.g.,
validation, execution of the contract code, is done by a custom validator and
interpreter. These are implemented in Rust in the
[smart-contracts](https://github.com/Concordium/concordium-wasm-smart-contracts/) repository.

## NRG

Since smart contracts are general programs that are inherently untrusted we must
prevent resource exhaustion by expensive, or even unbounded computations.
The Wasm runtime keeps track of a resource measure called "interpreter energy".
Because NRG is not granular enough, interpreter energy is not the same as NRG,
but there is a conversion factor, currently 1000 interpreter energy is 1 NRG.

Resource accounting is achieved via a program transformation that adds
additional instructions to the source program. These instructions charge
interpreter energy for each straight-line block of code. This is achieved via
calls to host functions. Hence the final artifact has, in addition to the host
functions listed below, also host functions for keeping track of NRG.

## Implementation details

The general design at present is that all the runtime structures are maintained
in the Rust heap and the Haskell runtime maintains pointers to these structures.
It also manages their lifetime and deallocates them when needed. This includes
- module artifacts. This is a processed Wasm module that is easier and quicker
  to execute. It is designed to be cheap to look up and deserialize from
  persistent storage.
- for V1 contracts a suspended state of the execution. When a contract invokes
  an operation we must suspend its execution and store the state so it can be
  resumed.

The execution of a contract invocation is initiated by the scheduler which
invokes the relevant functions through FFI. Execution then proceeds as far as
possible. For V0 contracts this means until termination of the initialization
function or contract entrypoint, however for V1 contracts execution can also be
interrupted. Upon this control is handed back to the scheduler which parses the
response and decides what needs to be done next.

The scheduler also manages the state of the contract, its checkpointing, and
rollbacks.

# V0 contracts

TODO: Write information about V0 interface, restrictions.

# V1 contracts

V1 contracts differ from V0 contracts in both the way their state is managed,
and in the way they interact with other contracts.

The following host functions are available to V1 contracts. The host functions
are written as type signature in Rust syntax. The mapping of types is
- `u32`/`i32` in Rust are `i32` in Wasm
- `u64`/`i64` in Rust are `i64` in Wasm
- `*const S`/`*mut S` for a sized S are `i32` in Wasm (since Wasm is a 32-bit platform)

## Invoke an operation
```rust
/// Invoke a host instruction. The arguments are
///
/// - `tag`, which instruction to invoke
///   - 0 for transfer to account
///   - 1 for call to a contract
/// - `start`, pointer to the start of the invoke payload
/// - `length`, length of the payload
/// The return value positive indicates success or failure, and additional
/// information in those cases. The complete specification is as follows
/// - if the last 5 bytes are 0 then the call succeeded. In this case the first
///   bit of the response indicates whether our own state has changed (1) or not (0)
///   the remaining 23 bits are the index of the return value that can be used
///   in a call to `get_parameter_section` and `get_parameter_size`.
/// - otherwise
///   - if the fourth byte is 0 the call failed because of a logic error and
///     there is a return value. Bits 1..24 of the response are the index of the
///     return value. Bits 32..64 are to be interpreted in two's complement and
///     will be a negative number indicating the error code.
///   - otherwise only the fourth byte is set.
///   - if it is 1 then call failed due to transfer of non-existent amount
///   - if it is 2 then the account to transfer to did not exist
///   - if it is 3 then the contract to invoke did not exist
///   - if it is 4 then the entrypoint did not exist
///   - if it is 5 then sending a message to V0 contract failed.
///   - if it is 6 then invoking a contract failed with a runtime error
///   - no other values are possible
fn invoke(tag: u32, start: *const u8, length: u32) -> u64;
```

When a contract invokes another two things might happen to the contract itself
as a result of the call
- its state can change. This can happen due to reentrancy.
- its own balance can change. This can happen either because the contract is
  transferring some of its tokens or, again, because of reentrancy.

When an invocation fails none of these are changed, and any accrued changes are
rolled back.

If an invocation succeeds, then in the return value there is an indication on
whether the state might have changed or not. "Might have changed" is currently
defined as any of contract's entrypoints being executed successfully in the
middle of handling the interrupt.

TODO: The semantics of this will change with new state.

There is no direct indication of whether the balance has changed. However
**after** the invoke call, the host function `get_receive_self_balance` returns
the **new** self balance.

## Returning from a contract

A contract can produce a return value, in addition to the return code. The
return value is modelled as a writable buffer. If nothing is written, it is
empty.

```rust
/// Write to the return value of the contract. The parameters are
///
/// - `start` the pointer to the location in memory where the data resides
/// - `length` the size of data (in bytes)
/// - `offset` where in the return value to write the data
///
/// The return value indicates how many bytes were written.
fn write_output(start: *const u8, length: u32, offset: u32) -> u32;
```

## Parameters and observing return values from calls

Each contract invocation starts with a parameter. If then a contract invokes
another contract, which produces a return value, that return value is added to
the stack of parameters the original contract can observe.

```rust
/// Get the size of the `i`-th parameter to the call. 0-th parameter is
/// always the original parameter that the method was invoked with,
/// calling invoke adds additional parameters to the stack. Returns `-1`
/// if the given parameter does not exist.
fn get_parameter_size(i: u32) -> i32;
/// Write a section of the `i`-th parameter to the given location. Return
/// the number of bytes written or `-1` if the parameter does not exist.
/// The location is assumed to contain enough memory to
/// write the requested length into.
fn get_parameter_section(
    i: u32,
    param_bytes: *mut u8,
    length: u32,
    offset: u32,
) -> i32;
```

## Obtaining information about the invoker

Obtain policies on all credentials of the invoker. If the invoker is a contract
it is the credentials of the owner that are returned. If it is an account, then
the credentials of the account are returned.

```rust
/// Write a section of the policy to the given location. Return the number
/// of bytes written. The location is assumed to contain enough memory to
/// write the requested length into.
fn get_policy_section(policy_bytes: *mut u8, length: u32, offset: u32) -> u32;
```

## Logging

Contracts can output a small amount of data during execution. This data is
available in a transaction status and can be queried. Concretely contracts can
output up to 64 items, each of at most 512 bytes (this is controlled by
constants `MAX_LOG_SIZE` and `MAX_NUM_LOGS` in the `constants.rs` module in the
`wasm-chain-integration` package)

```rust
/// Add a log item. Return values are
/// - -1 if logging failed due to the message being too long
/// - 0 if the log is already full
/// - 1 if data was successfully logged.
fn log_event(start: *const u8, length: u32) -> i32;
```

## State operations

The state exposes a key-value store with arbitrary byte arrays as values.
Internally the tree is implemented as a radix tree with branching on 4 bits, but
the interface that is exposed is a key-value store. Iteration is supported over
subtrees anchored at specific prefixes.

When looking up, or iterating over the tree entries are given out to the
contract. An entry is essentially a pointer to the tree node. The reason for
this desing is in major part because in Wasm only primitive (i64 or i32) values
may be returned by functions. In order to make lookup possible and practical to
wrap in Rust (and other high-level languages) we return entries, which are
encoded as 64 bit integers. Then these entries may be used to read and write the
value at the tree node. In concordium-std the interface that is exposed is the
Read + Write traits (and Seek).

This separation of operations into two steps also affects costs. Lookup (and
creation and deletion) costs are based on the length of the key, but further
operations on the entry do not depend on the length of the key, but only on the
data that is being written or read.

There is a non-trivial interaction between entries and contract calls. When a
contract A invokes contract B, that might lead to contract A's state being
modified (e.g., B = A, or B further calls C, ...). This means that if an entry
is given out before the call, it might no longer be valid after the call (e.g.,
the value at the key might have been deleted). For this reason if as a result of
a contract call contract A's state was modified (which is defined as any of the
state modification functions being called) then all the entries that were given
out are invalidated and attempting to use them will lead to an error return
value.

### Lookup, creation, and deletion
```rust
    /// Lookup an entry with the given key. The return value is either
    /// u64::MAX if the entry at the given key does not exist, or else
    /// the first bit of the result is 0, and the remaining bits
    /// are an entry identifier that may be used in subsequent calls.
    pub(crate) fn state_lookup_entry(key_start: *const u8, key_length: u32) -> u64;

    /// Create an empty entry with the given key. The return value is either
    /// u64::MAX if creating the entry failed because of an iterator lock on
    /// the part of the tree, or else the first bit is 0, and the remaining
    /// bits are an entry identifier that maybe used in subsequent calls.
    /// If an entry at that key already exists it is set to the empty entry.
    pub(crate) fn state_create_entry(key_start: *const u8, key_length: u32) -> u64;

    /// Delete the entry. Returns one of
    /// - 0 if the part of the tree this entry was in is locked
    /// - 1 if the entry did not exist
    /// - 2 if the entry was deleted as a result of this call.
    pub(crate) fn state_delete_entry(key_start: *const u8, key_length: u32) -> u32;

    /// Delete a prefix in the tree, that is, delete all parts of the tree that
    /// have the given key as prefix. Returns
    /// - 0 if the tree was locked and thus deletion failed.
    /// - 1 if the tree **was not locked**, but the key points to an empty part
    ///   of the tree
    /// - 2 if a part of the tree was successfully deleted
    pub(crate) fn state_delete_prefix(key_start: *const u8, key_length: u32) -> u32;
```

### Iteration
```rust
/// Construct an iterator over a part of the tree. This **locks the part of
/// the tree that has the given prefix**. Locking means that no
/// deletions or insertions of entries may occur in that subtree.
/// Returns
/// - all 1 bits if too many iterators already exist with this key
/// - all but second bit set to 1 if there is no value in the state with the
///   given key
/// - otherwise the first bit is 0, and the remaining bits are the iterator
///   identifier
/// that may be used in subsequent calls to advance it, or to get its key.
fn state_iterate_prefix(prefix_start: *const u8, prefix_length: u32) -> u64;

/// Return the next entry along the iterator, and advance the iterator.
/// The return value is
/// - u64::MAX if the iterator does not exist (it was deleted, or the ID is
///   invalid)
/// - all but the second bit set to 1 if no more entries are left, the
///   iterator
/// is exhausted. All further calls will yield the same until the iterator
/// is deleted.
/// - otherwise the first bit is 0, and the remaining bits encode an entry
///   identifier that can be passed to any of the entry methods.
fn state_iterator_next(iterator: u64) -> u64;

/// Delete the iterator, unlocking the subtree. Returns
/// - u64::MAX if the iterator does not exist.
/// - 0 if the iterator was already deleted
/// - 1 if the iterator was successfully deleted as a result of this call.
fn state_iterator_delete(iterator: u64) -> u32;

/// Get the length of the key that the iterator is currently pointing at.
/// Returns
/// - u32::MAX if the iterator does not exist
/// - otherwise the length of the key in bytes.
fn state_iterator_key_size(iterator: u64) -> u32;

/// Read a section of the key the iterator is currently pointing at. Returns
/// either
/// - u32::MAX if the iterator has already been deleted
/// - the amount of data that was copied. This will never be more than the
///   supplied length.
/// Before the first call to the [state_iterator_next] function this returns
/// (sections of) the key that was used to create the iterator. After
/// [state_iterator_next] returns (the encoding of) [None] this method
/// returns (sections of) the key at the first node returned by the
/// iterator.
fn state_iterator_key_read(
    iterator: u64,
    start: *mut u8,
    length: u32,
    offset: u32,
) -> u32;
```

### Reading and writing of node values.
```rust
// Operations on the entry.

/// Read a part of the entry. The arguments are
/// entry ... entry id returned by state_iterator_next or state_create_entry
/// start ... where to write in Wasm memory
/// length ... length of the data to read
/// offset ... where to start reading in the entry
/// The return value is
/// - u32::MAX if the entry does not exist (has been invalidated, or never
/// existed). In this case no data is written.
/// - amount of data that was read. This is never more than length.
fn state_entry_read(entry: u64, start: *mut u8, length: u32, offset: u32) -> u32;

/// Write a part of the entry. The arguments are
/// entry ... entry id returned by state_iterator_next or state_create_entry
/// start ... where to read from Wasm memory
/// length ... length of the data to read
/// offset ... where to start writing in the entry
/// The return value is
/// - u32::MAX if the entry does not exist (has been invalidated, or never
/// existed). In this case no data is written.
/// - amount of data that was written. This is never more than length.
fn state_entry_write(entry: u64, start: *const u8, length: u32, offset: u32) -> u32;

/// Return the current size of the entry in bytes.
/// The return value is either
/// - u32::MAX if the entry does not exist (has been invalidated, or never
/// existed). In this case no data is written.
/// - or the size of the entry.
fn state_entry_size(entry: u64) -> u32;

/// Resize the entry to the given size. Returns
/// - u32::MAX if the entry has already been invalidated
/// - 0 if the attempt was unsuccessful because new_size exceeds maximum
///   entry size
/// - 1 if the entry was successfully resized.
fn state_entry_resize(entry: u64, new_size: u32) -> u32;
```

## Chain metadata

Time of the block is available to all methods. This is the only chain metadata
contracts can observe, e.g., block height and similar implementation details are
not available.

```rust
/// Slot time (in milliseconds) of the block the transaction is being executed in.
fn get_slot_time() -> u64;
```

## Caller context available only to init methods

```rust
/// Address of the sender, 32 bytes.
/// The address is written to the provided location, which must be able to hold
/// 32 bytes.
fn get_init_origin(start: *mut u8);
```

## Caller context available only to receive methods

```rust
/// Invoker of the top-level transaction, AccountAddress.
/// The address is written to the provided location, which must be able to hold
/// 32 bytes.
fn get_receive_invoker(start: *mut u8);
/// Address of the contract itself, ContractAddress.
/// The address is written to the provided location, which must be able to hold
/// 16 bytes.
fn get_receive_self_address(start: *mut u8);
/// Self-balance of the contract, returns the amount in microCCD.
fn get_receive_self_balance() -> u64;
/// Immediate sender of the message (either contract or account).
/// The address is written to the provided location, which must be able to hold
/// 33 bytes.
fn get_receive_sender(start: *mut u8);
/// Owner of the contract, AccountAddress.
/// The address is written to the provided location, which must be able to hold
/// 32 bytes.
fn get_receive_owner(start: *mut u8);
```

## Fallback entrypoint handling

A V1 contract may have a fallback entrypoint. This is by definition the
entrypoint with the empty name, e.g., for a contract named `foo` the fallback
entrypoint has a receive name `foo.`. The semantics of fallback entrypoints is
that if a method on a contract is invoked, if there is no exact match for an
entrypoint, then the fallback entrypoint is invoked, if it exists.

Since this means that the executed entrypoint may be different from the one that
was named in, e.g., transaction, it is useful to expose the named entrypoint in
the contract. That is the purpose of the following two methods which are only
allowed for receive methods. Invoking these in an init method will lead to a
runtime error and contract termination.

```rust
/// Get the size of the entrypoint that was named.
fn get_receive_entrypoint_size() -> u32;

/// Write the receive entrypoint name into the given location.
/// It is assumed that the location contains enough space to write the name.
fn get_receive_entrypoint(start: *mut u8);
```
