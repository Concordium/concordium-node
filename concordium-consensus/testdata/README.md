To generate the data move into the `crypto/rust-src/simple_id_client` directory
and issue the following commands

```bash
mkdir database
cargo run --release --bin client -- generate-global
cargo run --release --bin client -- generate-ips
cargo run --release --bin generate_testdata --ip-data database/identity_provider-0.json
```

Then copy the files `database/global.json`, `database/identity_providers.json`,
and `credential-?.json` to this folder. Lastly copy the file
`identity_providers.json` to `identity-providers.json`
