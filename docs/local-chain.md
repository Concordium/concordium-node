# How to run a local chain

If you don't need to run a node (or local chain) from the source code, use the public docker images and guides:

[Guide to run published nodes](https://docs.concordium.com/en/mainnet/docs/network/nodes/node-requirements.html)

[Download Node Distributions](https://docs.concordium.com/en/mainnet/docs/installation/downloads.html#concordium-node-distributions)

## Outline:

To run a custom chain locally three things are needed
- a node
- genesis data
- at least one set of baker/validator credentials

## Prerequisites:

The node has two main parts. 

The first part is written in [Haskell](https://github.com/Concordium/concordium-node/tree/main/concordium-consensus) 
and contains functionality related to:
- consensus
- block and tree storage
- executing and filtering of blocks/transactions

The second part is writte in [Rust](https://github.com/Concordium/concordium-node/tree/main/concordium-node)
and contains functionality related to:
- networking 
- bootstrapping

Ensure you have all dependencies installed for these two languages:
- [Haskell dependencies](https://github.com/Concordium/concordium-node/tree/main/concordium-consensus#build-requirements)
- [Rust dependencies](https://github.com/Concordium/concordium-node/tree/main/concordium-node#dependencies-to-build-the-project): 
Install the recommended [rustup](https://www.rust-lang.org/tools/install) tool to get and manage Rust on your local machine.

## Step 1 (Genesis data):

Here we describe how to get or create the genesis block (first block). This block contains the genesis data which configures the chain at start-up.
This will include initial baker/validator credentials to configure certain nodes as bakers (validators).
The credentials are typically called `baker-?-credentials.json` (where `?` is the
baker/validator id).

### Option 1 (Generate the genesis data):

If you generate new genesis data, you start a new blockchain network. 
Your node will only be able to join this newly generated blockchain network. 
Meaning the node will not be able to join `devnet`, `stagenet`, `testnet`, or `mainnet`.

We have a
[genesis-creator](https://github.com/Concordium/concordium-misc-tools/tree/main/genesis-creator)
tool that can be used to create genesis blocks configured to specific scenarios.
The main output of the genesis tool that is needed for running the node is the
genesis block. This is typically called `genesis.dat`, and it is a binary file
in a custom protocol-dependent format.

Please see the README in the above repository for how to build and use the ``genesis-creator`` tool for custom set-ups.

For the most basic set-up (one local node running the newest protocol version), 
the recommendation is to use the newest example which at the time of writing is the protocol 
[version 9](https://github.com/Concordium/concordium-misc-tools/blob/main/genesis-creator/examples/genesis9.toml). 
If you want to start a basic network with just one node, 
it is recommended to change the number of validators in the `genesis9.toml` file to 1.

E.g. change
```
[[accounts]]
kind = "fresh"
balance = "1000000000000000"
stake = "500000000000000"
template = "baker"
identityProvider = 0
numKeys = 1
threshold = 1
repeat = 5
```

to 

```
[[accounts]]
kind = "fresh"
balance = "1000000000000000"
stake = "500000000000000"
template = "baker"
identityProvider = 0
numKeys = 1
threshold = 1
repeat = 1
```

You can now generate the genesis data:

```
cd concordium-misc-tools/genesis-creator
cargo run -- generate --config ./examples/genesis9.toml
```

### Option 2 (Get the genesis data):

- Only for internal Concordium members/employees (private repo):
Look up the `devnet`, `stagenet`, `testnet` or `mainnet` genesis data to start your node. 
This will allow your node to join the respective network and you can re-use funded (or specially authorized) genesis keys/accounts for testing 
[here](https://github.com/Concordium/concordium-infra-genesis-data)

- For the public:
Look up the `testnet` or `mainnet` genesis data to start your node [here](https://docs.concordium.com/en/mainnet/docs/installation/downloads.html#genesis-block)

## Step 2:

Here we describe how to get the source code for building the node.

Clone the node repo and initialize the submodule dependencies:
```
git clone --recurse-submodules git@github.com:Concordium/concordium-node.git
```

## Step 3 (Haskell dependencies of the node):

Here we describe how to build the [consensus part](https://github.com/Concordium/concordium-node/tree/main/concordium-consensus) 
of the node which is written in Haskell. Everytime files in the `consensus` folder get changed, re-build the Haskell dependencies 
of the node before running the node.

```
stack build
```

## Step 4 (Rust dependencies of the node):

Here we describe how to build and run the [node](https://github.com/Concordium/concordium-node/tree/main/concordium-node#building-the-node) 
and setting up a local network.

To start a node using a given genesis block file located at `/path/to/genesis.dat` do the
following (note that the path must either be **absolute** or **relative to the
data directory**).

- Create a database directory for the node, say `node-0`.
- Build the node. In the following example commands the assumption is that the
  node is being run from the
  [concordium-node](https://github.com/Concordium/concordium-node/tree/main/concordium-node)
  directory. Otherwise replace the `cargo run --release --` with the path to the
  `concordium-node` executable.
- Run the node supplying the genesis.

```console
cargo run --release -- \
   --genesis-data-file /path/to/genesis.dat \
   --no-bootstrap= \
   --listen-port 8000 \
   --grpc2-listen-port 7000 \
   --grpc2-listen-addr 127.0.0.1 \
   --data-dir node-0 \
   --config-dir node-0 \
   --debug=
```

This will start a node that can be connected to at port `8000`, and has its GRPC
V2 interface open at port 7000.

- To run a validator using validator credentials (keys) located at
`/path/bakers/baker-0-credentials.json`  run

```console
cargo run --release -- \
   --genesis-data-file /path/to/genesis.dat \
   --no-bootstrap= \
   --listen-port 8000 \
   --grpc2-listen-port 7000 \
   --grpc2-listen-addr 127.0.0.1 \
   --data-dir node-0 \
   --config-dir node-0 \
   --validator-credentials-file /path/bakers/baker-0-credentials.json \
   --debug=
```

## Running multiple nodes

Typically the reason for running a local node is to test some new development.
If more than one validator is specified in genesis then for the chain to run
correctly you will typically have to start multiple nodes and connect them
together so that they can build a single chain. To start two nodes connected to
each other run the following two commands from different terminals.

### Option 1 (Use cli commands):

```console
cargo run --release -- \
   --genesis-data-file /path/to/genesis.dat \
   --no-bootstrap= \
   --listen-port 8000 \
   --grpc2-listen-port 7000 \
   --grpc2-listen-addr 127.0.0.1 \
   --data-dir node-0 \
   --config-dir node-0 \
   --validator-credentials-file /path/bakers/baker-0-credentials.json \
   --connect-to 127.0.0.1:8001 \
   --debug=
```

The `--connect-to 127.0.0.1:8001` option tells the node to try to connect to
another node at `localhost` on port `8001`. This is where we will start the
second node.

```console
cargo run --release -- \
   --genesis-data-file /path/to/genesis.dat \
   --no-bootstrap= \
   --listen-port 8001 \
   --grpc2-listen-port 7001 \
   --grpc2-listen-addr 127.0.0.1 \
   --data-dir node-1 \
   --config-dir node-1 \
   --validator-credentials-file /path/bakers/baker-1-credentials.json \
   --connect-to 127.0.0.1:8000 \
   --debug=
```

Note that the second node listens on port `8001` (resp `7001`) instead of `8000`
(resp `7000`) so that both can run at the same time.

### Option 2 (Use shell scripts):

As orchestrating several nodes will become tedious, the following shell script might be used for simplifying 
the setup process of a local network with multiple nodes. 

Running the first validator:
```
./network-setup.sh 0 --connect-to 0.0.0.0:8001
```

Running the second validator and connecting it to the network:
```
./network-setup.sh 1 --connect-to 0.0.0.0:8000
```

where the `network-setup.sh` is the following shell script. Update the `[path-on-your-machine]` 
value and the envionmental variables `GENESIS_DATA_FILE_PATH` and `NODE_VALIDATOR_PATH` respectively.

```
#!/usr/bin/env bash

set -euo pipefail
IFS=$'\n\t'

USAGE="Usage: $0 <validator-id> [additional args]"

if [ "$#" -lt 1 ]; then
   echo "$USAGE"
   exit 1
fi
VALIDATOR_ID=$1
shift

# Ports adjusted per validator ID
NODE_PORT=$((8000 + VALIDATOR_ID))
GRPC_PORT=$((7000 + VALIDATOR_ID))

NODE_CONFIG_DIR=./config_dir/validator-$VALIDATOR_ID
# The genesis.dat file needs to be specified relative to the NODE_CONFIG_DIR
GENESIS_DATA_FILE_PATH=../../../../../concordium-misc-tools/genesis-creator/genesis.dat
NODE_VALIDATOR_PATH=../../../concordium-misc-tools/genesis-creator/bakers/baker-$VALIDATOR_ID-credentials.json

cd ./[path-on-your-machine]/concordium-node/concordium-node
mkdir -p $NODE_CONFIG_DIR
cargo run -- \
    --genesis-data-file $GENESIS_DATA_FILE_PATH \
    --no-bootstrap true \
    --listen-port $NODE_PORT \
    --listen-address "0.0.0.0" \
    --grpc2-enable-grpc-web true \
    --grpc2-listen-addr "0.0.0.0" \
    --grpc2-listen-port $GRPC_PORT \
    --data-dir $NODE_CONFIG_DIR \
    --config-dir $NODE_CONFIG_DIR \
    --validator-credentials-file $NODE_VALIDATOR_PATH \
    --debug true \
    "$@"
```

Note:
If your genesis data specified only one valiator, your running node should immediatly start the consensus algorithm.
If your genesis data specified several valiators, your nodes will start the consensus algorithm 
once enough validators (stake) have joined the network.

## Step 5:

Interact with your running node via its GRPC V2 interface.

[grpc API repo](https://github.com/Concordium/concordium-grpc-api)

[grpc API documentation](https://docs.concordium.com/concordium-grpc-api/)

We recommend to download the graphic UI [grpcui](https://github.com/fullstorydev/grpcui) to interact with the node.

e.g.
```
grpcui -plaintext -import-path ./[path-on-your-machine]/concordium-grpc-api/ -proto ./[path-on-your-machine]/concordium-grpc-api/v2/concordium/service.proto "localhost:7000"
```

Note: If you want to interact with a remote node that has `TLS` enabled, remove the `-plaintext` option in above command.

## Step 6 (Optional):

You can also interact with your running node via the command-line-tool `concordium-client` 
which can be downloaded [here](https://docs.concordium.com/en/mainnet/docs/installation/downloads.html#concordium-client-client-version).

e.g.
```
concordium-client consensus status --grpc-port 7000 --grpc-ip 0.0.0.0
```

## Step 7 (Optional):

You can also interact with your running node via the SDKs:
- [Node/Web/Javascript/Typescript SDK](https://www.npmjs.com/package/@concordium/web-sdk)
- [Rust SDK](https://docs.rs/concordium-rust-sdk/latest/concordium_rust_sdk/)

## Caveats when running a node locally

- If a genesis time is too far in the past (e.g., a year ago) then the node might
take a long time to produce the first block.

- When starting multiple validators, if there is sufficient delay between starting
  the different nodes then some initial messages that validators send might be lost.
  Catchup will ensure that blocks are correctly propagated when the nodes
  initially connect, but sent finalization messages are not part of catchup.
  Instead finalization catchup is based on replay where a node resends relevant
  messages after a period of no activity. This duration is 5min at present.
  
  This can mean that it takes a while for initial finalization to happen,
  depending on the timing of node startup.
  
- There is a minimum amount of CCD that is needed to be a finalizer. In the
  example configurations we typically use 1/1000 of total CCD. Make sure that
  when generating a sample genesis enough stake is dedicated to validators so that
  there is a finalization committee.
