# P2P Layer implementation in Rust
This repository uses git lfs for storing binary dependencies. Therefore the git lfs extension needs to be installed.
Installation instructions can be found here: https://git-lfs.github.com/

After installing the extension set it up using
`git lfs install`

Proceed as normally with git


## Dependencies to build the project
* Rust (nightly)
* openssl >= 1.0.1
* cmake
* go >=1.10
* protobuf >= 3.5.1
* [Unbound](https://www.nlnetlabs.nl/projects/unbound/about/)
* libclang >= 6.0
* [HACL*](https://github.com/mitls/hacl-c)
* Stack (GHC-8.4.4)

## Setting up basic local build environment
Install the needed dependencies from the list above (Windows build is special, for that see cross-compilation build environment setup script in scripts/init.win.build.env.sh for further details), and run the script (requires that the user executing is has sudo privileges) `scripts/local-setup-unix-deps.sh` and pay special attention to setting the right version of GHC (see [build scripts](https://gitlab.com/Concordium/p2p-client/blob/master/scripts/init.build.env.sh#L10) for details).

## Running the library as a binary (usable via RPC)
`cargo run -- --debug --private-mode`

## Running the two node test case

### Node 1
`cargo run --example p2p_node_1 -- -i c19cd000746763871fae95fcdd4508dfd8bf725f9767be68c3038df183527bb2 --debug --private-mode`

### Node 2
`cargo run --example p2p_node_2 -- -l 8889 -c 127.0.0.1:8888 --private-mode`

## Running all tests
`cargo test --all`

## Creating the default node docker image
`scripts/docker.node.basic.sh`


## Building locally with Docker

Run `./scripts/build-local.sh` and on successful build run;

```
# Example for localhost with 2 nodes
$ ./scripts/start-local-test.sh 127.0.0.1 2
--> Starting bootstrapper...
c00a2ec87fec404ea38c7265d186a5ba7fb00fe010c9640243d534fceaeb6adf
--> Bootstrapper started, waiting 5s...
--> Starting node on port 8890
b01b9cef07cc7b8ec984f571d214d6ece8fa347d60204837109fe3dfc976efaf
--> Starting node on port 8891
b7154a37ce3e4ee6b1af6875a6866719e7ef6505eacb289c0e72c822e718028b
```

If you want to enable gRPC, you'll need to make this change and rebuild your docker images:

```diff
diff --git a/scripts/local.testnode.Dockerfile b/scripts/local.testnode.Dockerfile
index 91ec912..13674e1 100644
--- a/scripts/local.testnode.Dockerfile
+++ b/scripts/local.testnode.Dockerfile
@@ -4,7 +4,7 @@ COPY ./scripts/local-start-node.sh ./start-node.sh
 RUN chmod +x ./start-node.sh
 EXPOSE 10000
 EXPOSE 8888
-ENV EXTRA_ARGS="--debug"
+ENV EXTRA_ARGS="--debug --rpc-server-addr 0.0.0.0"
 ENV DESIRED_PEERS="10"
 ENV EXTERNAL_PORT="8889"
 ENV BOOTSTRAP_NODE="127.0.0.1:8888"
```
