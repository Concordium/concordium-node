# Protocol-level token (PLT) Scheduler

[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.0-4baaaa.svg)](https://github.com/Concordium/.github/blob/main/.github/CODE_OF_CONDUCT.md)
![Build and test](https://github.com/Concordium/concordium-node/actions/workflows/plt-scheduler-build-test.yaml/badge.svg)

This project implements the scheduler (block item execution) for protocol-level tokens in Rust.
The present Rust implementation is currently used on P11 and upwards. The Haskell implementation is used on P9 and P10.
This may be changed, such that the Rust implementation is also used on P9 and P10.

The PLT Scheduler is compiled as a native library and is used within the Haskell implemented scheduler found as part of [`concordium-consensus`](../concordium-consensus/README.md).
See [PLT Scheduler Component View](docs/component_view.md).

The crates in the project are:
* `plt-types`: Externally facing scheduler and query types. These types correspond to identical types on the Haskell side.
* `plt-scheduler-interface`: The internally facing scheduler interface seen by the token module, e.g. the token kernel.
* `plt-token-module`: The Token Module implementation.
* `plt-scheduler`: The actual PLT Scheduler implementation.
