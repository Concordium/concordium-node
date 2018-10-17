# Haskell + Rust

The following steps seem to work on Windows to build the Haskell code and the simple Rust host.

1. In `Concordium`, run `stack build`.  This should produce `HSdll.dll` and `HSdll.dll.a`.
2. Rename `HSdll.dll.a` to `HSdll.lib`.
3. Copy `HSdll.dll` to `RustRunner`.
4. In `RustRunner`, run `cargo run`.
