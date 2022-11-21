# How to run a local chain

To run a custom chain three things are needed
- a node
- genesis
- at least one set of baker credentials

The instructions for building your own node are in [a separate
document](https://github.com/Concordium/concordium-node/tree/main/concordium-node#building-the-node).

Here we describe how to build a genesis block and start the node(s) with the
given genesis.

We have a
[genesis-creator](https://github.com/Concordium/concordium-misc-tools/tree/main/genesis-creator)
tool that can be used to create genesis blocks configured to specific scenarios.
Please see the README in the linked repository for how to build and use the tool.

The main output of the genesis tool that is needed for running the node is the
genesis block. This is typically called `genesis.dat`, and it is a binary file
in a custom protocol-dependent format.

To run a local chain you will also need baker credentials to make some nodes
bakers. These are typically called `baker-?-credentials.json` (where `?` is the
baker id) and are generated as part of genesis block creation.

To start a node using such a file located at `/path/to/genesis.dat` do the
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
   --rpc-server-port 7000 \
   --data-dir node-0 \
   --config-dir node-0 \
   --debug=
```

This will start a node that can be connected to at port `8000`, and has its GRPC
V1 interface open at port 7000.

- To run a baker using baker credentials (keys) located at
`/path/bakers/baker-0-credentials.json`  run

```console
cargo run --release -- \
   --genesis-data-file /path/to/genesis.dat \
   --no-bootstrap= \
   --listen-port 8000 \
   --rpc-server-port 7000 \
   --data-dir node-0 \
   --config-dir node-0 \
   --baker-credentials-file /path/bakers/baker-0-credentials.json \
   --debug=
```

## Running multiple nodes

Typically the reason for running a local node is to test some new development.
If more than one baker is specified in genesis then for the chain to run
correctly you will typically have to start multiple nodes and connect them
together so that they can build a single chain. To start two nodes connected to
each other run the following two commands from different terminals.

### Run the first baker node

```console
cargo run --release -- \
   --genesis-data-file /path/to/genesis.dat \
   --no-bootstrap= \
   --listen-port 8000 \
   --rpc-server-port 7000 \
   --data-dir node-0 \
   --config-dir node-0 \
   --baker-credentials-file /path/bakers/baker-0-credentials.json \
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
   --rpc-server-port 7001 \
   --data-dir node-1 \
   --config-dir node-1 \
   --baker-credentials-file /path/bakers/baker-1-credentials.json \
   --connect-to 127.0.0.1:8000 \
   --debug=
```

Note that the second node listens on port `8001` (resp `7001`) instead of `8000`
(resp `7000`) so that both can run at the same time.

## Caveats

- If a genesis time is too far in the past (e.g., a year ago) then the node might
take a long time to produce the first block.

- When starting multiple bakers, if there is sufficient delay between starting
  the different nodes then some initial messages that bakers send might be lost.
  Catchup will ensure that blocks are correctly propagated when the nodes
  initially connect, but sent finalization messages are not part of catchup.
  Instead finalization catchup is based on replay where a node resends relevant
  messages after a period of no activity. This duration is 5min at present.
  
  This can mean that it takes a while for initial finalization to happen,
  depending on the timing of node startup.
  
- There is a minimum amount of CCD that is needed to be a finalizer. In the
  example configurations we typically use 1/1000 of total CCD. Make sure that
  when generating a sample genesis enough stake is dedicated to bakers so that
  there is a finalization committee.
