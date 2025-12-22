# concordium-node: Deployment unit for Protocol-level token (PLT)

[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.0-4baaaa.svg)](https://github.com/Concordium/.github/blob/main/.github/CODE_OF_CONDUCT.md)
![Build and test](https://github.com/Concordium/concordium-node/actions/workflows/deployment-build-test.yaml/badge.svg)

This project implements the runtime upgradable PLT deployment unit introduced as part of Concordium Protocol Version 10.

The PLT deployment unit is written as a Rust library which is then compiled to [WebAssembly](https://webassembly.org/) and can be deployed on the Concordium blockchain by the Governance Committee.
