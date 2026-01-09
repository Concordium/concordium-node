# concordium-node: Scheduler implementation for Protocol-level token (PLT)

[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.0-4baaaa.svg)](https://github.com/Concordium/.github/blob/main/.github/CODE_OF_CONDUCT.md)
![Build and test](https://github.com/Concordium/concordium-node/actions/workflows/plt-scheduler-build-test.yaml/badge.svg)

This crate provides a scheduler (transaction execution) implementation for concordium-node, and currently only supports transactions related to Protocol-level Tokens.

It is compiled as a native library and is used within the Haskell implemented scheduler found as part of [`concordium-consensus`](../concordium-consensus/README.md).
