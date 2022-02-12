# Example contracts

This directory and its subdirectories contain a number of wasm modules that are
used to test integration of the smart contract execution engine with the
scheduler.

These modules are generally written manually in Web Assembly Text format (`.wat`)
and then transformed to `.wasm` files using the `wat2wasm` tool, which is part
of the [web assembly binary toolkit](https://github.com/WebAssembly/wabt).

Each of the `.wat` files should start with a header briefly explaining the
contents of the module, and what it is designed to test.
