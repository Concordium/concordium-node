# P2P Layer implementation in Rust
## Dependencies to build the project
* Rust (nightly)
* openssl
* [HACL*](https://github.com/mitls/hacl-c)
## Running the library as a binary (usable via RPC)
`RUST_LOG=info cargo run -- --debug`
## Running the two node test case
Node 1:
`RUST_LOG=info cargo run --example p2p_node_1 -- -i c19cd000746763871fae95fcdd4508dfd8bf725f9767be68c3038df183527bb2 --debug`
Node 2:
`RUST_LOG=info cargo run --example p2p_node_2 -- -p 8889`
## Running all tests
`cargo test --all`