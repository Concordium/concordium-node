# Test data generation.

All the tools used reside in [../../concordium-base/rust-bins](../../concordium-base/rust-bins) in concordium-base.
It is assumed that all commands are run directly from the `testdata` directory.

First generate `global.json` or reuse the existing one [./global.json](./global.json) if no change is needed.
```console
client generate-global --out-file global.json
```

**It is important that all subsequent parameters are generated with a consistent set of global parameters.**

## Initial identity providers and anonymity revokers

The full command to generate a fresh set of identity providers and anonymity revokers is
```console
client generate-ips --global global.json --key-capacity 30 --num 1 --num-ars 5 --out-dir .
```

will generate one identity provider and 5 anonymity revokers. In particular it
will output `identity_provider-0.json`,
[./anonymity_revokers.json](./anonymity_revokers.json), and
[./identity_providers.json](./identity_providers.json).

## Generate test credentials

The final command is

```console
generate_testdata --ip-data identity_provider-0.json --ars anonymity_revokers.json --global global.json
```

which will output all `credential-*.json`, `initial-credential-*.json` and
`credential-private-keys-*.json` files.
