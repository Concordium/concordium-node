# P2P Layer implementation in Rust
## General usage information
This repository uses git lfs for storing binary dependencies, and relies on git submodules for internal component dependencies. Therefore the git lfs extension needs to be installed, and to update the code remember to pull submodules too recursively.

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

## Running the library as a binary (usable via gRPC)
```bash
$> cargo run -- --debug --private-mode
```

## Running all tests
```
$> cargo test --all
```

## Running a complete network locally
Use docker-compose and follow instructions in [scripts/local/README.md](https://gitlab.com/Concordium/p2p-client/tree/features/consensus_integration_master_branch/scripts/local)