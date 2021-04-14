# gRPC for interacting with Smart Contracts

*Documentation of our gRPC API and data types related to smart contracts.*

Documents the following gRPC calls:
- `GetAccountInfo`
- `GetInstances`
- `GetInstanceInfo`
- `SendTransaction`

A typical workflow is:

1. Deploy a wasm module by sending a `DeployModule` transaction.
2. Creating a contract instance of the module by sending a `InitContract`
   transaction.
3. Finding the address of the contract instance via `GetInstances` and
   `GetInstanceInfo`.
4. Updating the contract by sending a `Update` transaction.

## `GetAccountInfo`

*Retrieve information about an account.*

```protobuf 
rpc GetAccountInfo(GetAddressInfoRequest) returns (JsonResponse) {}

message GetAddressInfoRequest {
  string block_hash = 1;
  string address = 2;
}
```

**Input:**

- `block_hash`: A null-terminated base16 encoding of a block hash. 
- `address`: A null-terminated base58 encoding (same format as returned by `GetAccountList`) of an account address. 

**Output:**

- A JSON object containing information about the given address at the given block.
  - Example:

    ```json
    {"accountAmount":"10000000000000","accountNonce":1,"accountEncryptedAmount":{"incomingAmounts":[],"selfAmount":"c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000","startIndex":0},"accountEncryptionKey":"993fdc40bb8af4cb75caf8a53928d247be6285784b29578a06df312c28854c1bfac2fd0183967338b578772398d4120193c96e1a644ecb148615ec0940cdf30e2f373cc470f393231a4be681b8cbb27edf0b67fda7290a3545d62dc8038d1fa7","accountReleaseSchedule":{"schedule":[],"total":"0"},"accountCredentials":{"0":{"value":{"contents":{"ipIdentity":0,"regId":"93c96e1a644ecb148615ec0940cdf30e2f373cc470f393231a4be681b8cbb27edf0b67fda7290a3545d62dc8038d1fa7","policy":{"revealedAttributes":{},"createdAt":"202103","validTo":"202203"},"credentialPublicKeys":{"keys":{"0":{"verifyKey":"6bb6fb860514c7a1002fc9c3f3a9f7a7a518d54d0437b7dd7e9ba29c65263cd8","schemeId":"Ed25519"},"1":{"verifyKey":"80db411ba83f282ebe7ab3c037e481c72c460de1de87bc57a0b85a7dd347bd76","schemeId":"Ed25519"},"2":{"verifyKey":"c14a3e621d7ff5f1302a0b5c9371fbe8429e8db33b1ff33b962e468a901c159a","schemeId":"Ed25519"}},"threshold":2}},"type":"initial"},"v":0}}}
    ```

- Returns `null` if the address could not be found.

## `GetInstances`

*Retrieve the addresses of all smart contract instances.*

```protobuf
rpc GetInstances(BlockHash) returns (JsonResponse) {}

message BlockHash {
  string block_hash = 1;
}
```
**Input:**

- `block_hash`: A null-terminated base16 encoding of a block hash. 

**Output:**

- A JSON list of contract addresses on the chain.
  - Example: `[{"subindex":0,"index":0},{"subindex":0,"index":1}]` 

## `GetInstanceInfo`

*Retrieve information about a given smart contract instance.*

```protobuf
rpc GetInstanceInfo(GetAddressInfoRequest) returns (JsonResponse) {}

message GetAddressInfoRequest {
  string block_hash = 1;
  string address = 2;
}
```

**Input:**

- `block_hash`: A null-terminated base16 encoding of a block hash. 
- `address`: A null-terminated JSON-encoded value (same format as returned by
  `GetInstances`).

**Output:**

- A JSON object with information about the contract instance.
  - Example: 

  ```json
  {"amount":"0","sourceModule":"609af98562314a5461990080df5514703bfb9f98d2c5b4535cf4b11f766cac29","owner":"4NyqjcJwKAckGE3gMGDJeqcyLZQxtfnDFTdvg74xYUU8myQTrB","methods":["counter.receive","counter.receive_optimized"],"name":"init_counter","model":"0000000000"}
  ```


## `SendTransaction`

*Send a transaction.*

```protobuf
rpc SendTransaction(SendTransactionRequest) returns (BoolResponse) {}

message SendTransactionRequest {
  uint32 network_id = 1;
  bytes payload = 2;
}
```

**Input:**

- `network_id`: Id for the network. Default id is `100`.
- `payload`: Binary encoding of the transaction (details below).

**Output:**

- Boolean value indicating the transaction success.


What follows is the description of the data types, and their serialization, which is
needed to construct the `payload` in `SendTransaction`.
It starts with a `BlockItem` which is the type expected as `payload`.

### `BlockItem`

A `BlockItem` is a union type with a version number.
For interacting with smart contracts, the relevant variant is `AccountTransaction`.
    
#### Serialization

- Version (uint32, big-endian)
  - Only supported version is `0`.
- Tag (uint8)
  - Relevant tag values:
    - `0`: `AccountTransaction`
- `AccountTransaction` (see below)

### `AccountTransaction`

An `AccountTransaction` is a transaction that originates from a specific account
(the sender), and is paid for by the sender.

It is a struct with the following fields:

`TransactionSignature`: Signatures for the transaction. The message to sign is
the SHA256 of the `Payload`.

`TransactionHeader`: A header with common data needed for all types of
transactions.

`Payload`: The actual contents of the transaction. For smart contracts this is
`DeployModule`, `InitContract`, or `Update`.

#### Serialization

- `TransactionSignature` (see below)
- `TransactionHeader` (see below)
- `Payload` (see below)

### `TransactionSignature`
A transaction signature is map from the index of the credential to another map from the key index to the actual signature.
The credential index is relative to the account address, and the indices should be distinct.
The key index is relative to the credential.
The maximum length of the list is 255, and the minimum length is 1.
`TransactionSignature = Map CredentialIndex (Map KeyIndex Signature)`

#### Serialization
- Length of outer map (uint8)
- For each keypair in outer map:
  - `CredentialIndex` (uint8)
  - Length of inner map (uint8)
  - For each keypair in inner map:
    - `KeyIndex` (uint8)
    - Length of `Signature` in bytes (uint16, big-endian)
    - `Signature` as bytes

### `TransactionHeader`

A transaction header is a struct which consists of the following fields:

`AccountAddress`: The sender account.

`Nonce`: Account nonce. Is incremented by 1 with every transaction originating
from an account.

`Energy`: The amount of energy allocated for the execution of this transaction.

`PayloadSize`: Size of the `Payload` in bytes.

`TransactionExpiryTime`: Absolute expiration time after which transaction will
*not* be executed. Measured in seconds since unix epoch.

#### Serialization

- `AccountAddress` (byte array of size 32)
- `Nonce` (uint64, big-endian)
- `Energy` (uint64, big-endian)
- `PayloadSize` (uint32, big-endian)
- `TransactionExpiryTime` (uint64, big-endian)

### `Payload`
A `Payload` is a union type.
The three relevant variants for smart contracts are `DeployModule`,
`InitContract`, and `Update`.

#### Serialization

- Tag (uint8)
  - Relevant tag values:
    - `0`: `DeployModule`
    - `1`: `InitContract`
    - `2`: `Update`
- Content of variant (see below)

### DeployModule

`WasmModule` contains a version number and the binary module source.

#### Serialization

 - Version (uint32, big-endian)
 - Length of byte array (uint32, big-endian)
 - The byte array

### InitContract

`Amount`: Amount in microGTU (10^-6 GTU).

`ModuleRef`: Hash of the module on chain.

`InitName`: Name of init function including `init_` prefix.

`Parameter`: Parameter for the init function.

#### Serialization

- `Amount` (uint64, big-endian)
- `ModuleRef` (byte array of fixed size 32)
- `InitName`
  - Length of name in bytes (uint16, big-endian)
  - Name as a byte array
- `Parameter` (depends on the contract)

### Update (Contract)
`Amount`: Amount in microGTU (10^-6 GTU).

`ContractAddress`: Address of contract instance consisting of an index and a subindex.

`ReceiveName`: Name of receive function including `<contractName>.` prefix.

`Parameter`: Parameter for the receive function.

#### Serialization

- `Amount` (uint64, big-endian)
- `ContractAddress`
  - Index (uint64, big-endian)
  - Subindex (uint64, big-endian)
- `ReceiveName`
  - Length of name in bytes (uint64, big-endian)
  - Name as byte array
- `Parameter` (depends on the contract)
