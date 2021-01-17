# globalstate mockup

A Haskell mock-up of the global state component

## Notes on fractional numbers
See [globalstate-types/doc/Fractionals.md](globalstate-types/doc/Fractionals.md).

## Build requirements

### [Haskell Stack](https://docs.haskellstack.org/en/stable/README/) and [Rust](https://www.rust-lang.org/tools/install)

### lmdb
The global state depends on the lmdb library, which you may need to install using your system's package manager.
- On Windows, `stack` installs msys2.
  To install lmdb there, use the following command:
  ```
  stack exec -- pacman -S mingw-w64-x86_64-lmdb
  ```
- On Linux, install the `liblmdb-dev` package.
- On OS X, use `brew install lmdb`.

### PostgreSQL
Install `postgresql` as follows:
- On Windows:
  ```
  stack exec -- pacman -S mingw-w64-x86_64-postgresql
  ```
- On Linux, install the `postgresql-common` and `libpq-dev` packages
- On OS X:
  ```
  brew install postgresql libpq
  ```

## Troubleshooting

### `error: linking with link.exe failed` (Windows)
This might happen if Rust is installed with the MSVC [ABI](https://en.wikipedia.org/wiki/Application_binary_interface).
You can check this by running `rustup show`.
If Rust is using the MSVC toolchain you can switch to GNU instead by running
```
rustup default stable-x86_64-pc-windows-gnu
```

### `user specified .o/.so/.DLL could not be loaded (addDLL: pthread or dependencies not loaded. (Win32 error 5)) whilst trying to load:  (dynamic) pthread`
Copy `%APPDATA%\Local\Programs\stack\x86_64-windows\ghc-8.8.3\mingw\bin\libwinpthread-1.dll` to `%APPDATA%\Local\Programs\stack\x86_64-windows\ghc-8.8.3\mingw\bin\pthread.dll`.


# Out of band catchup using the database exporter

1. Export

When having a `data.mdb` file in path `$DATA`, we execute the tool providing an output path `$OUT`:
```
stack run database-exporter -- --dbpath $DATA --exportpath $OUT
```

This will generate a binary file that contains all the blocks written to the database excluding the genesis block. Each serialized block is prepended by its length in bytes.

2. Check

When having an exported database in path `$OUT` we execute the tool with the `-r` flag:
```
stack run database-exporter -- --exportpath $OUT -r
```

This will try to read the exported file showing the hash and block slot for each block read from the file. With this we can check that the exported data is well formed. Note that this doesn't check if the blocks are properly chained in sequence or any other integrity rule.

3. Import

When a baker is created and before starting the baker thread, we can provide the blocks in sequence and they will be imported. As the blocks are finalized, a consumer baker would reach the same point in the chain as the one that exported the blocks because the blocks carry the finalization information. An easy way to do this, integrated with the tools we have is providing the exported file to the `p2p-client` via the flag `--import-blocks-from <PATH>`. The `p2p-client` will retrieve the file and give it to consensus, who will import each serialized block in sequence. If any imports fail, the state will remain as-is and the node will have to catch-up after starting.
