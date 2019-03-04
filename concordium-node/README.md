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