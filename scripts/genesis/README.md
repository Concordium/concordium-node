# Genesis configurations

The following genesis configurations are available in this directory.

* [genesis-p1.json](./genesis-p1.json) for protocol versions 1-3
* [genesis.json](./genesis.json) for protocol version 4

These have the common configuration of:
   - slot duration 0.25s
   - average block time 10s
   - maximum block energy limit 3_000_000

## Docker

The dockerfile builds an image for generating genesis block and accounts.

Expected volume:
* `/work`: Should contain 'genesis.json'.

Expected env vars:
* `NUM_BAKERS`: Number of bakers to define.
* `GENESIS_DIR`: Output directory. Defaults to `/work/out`.

Supported env vars:
* `EXTRA_ACCOUNTS_TEMPLATE`, `NUM_EXTRA_ACCOUNTS`, `EXTRA_ACCOUNTS_BALANCE`:
  The naming to use for extra accounts (this enables them being generated), how many to create, and with what balance.
* `DROP_ACCOUNT_BALANCE` will, if set to a CCD amount, generate another extra account in the
  folder `drop`. Note that if you set this and `EXTRA_ACCOUNTS_TEMPLATE` then
  the latter is different from `drop`.
* `GENESIS_VERSION` sets the genesis version to use. This should be 2 more than the protocol version
  used at genesis, and defaults to 6 (protocol version 4) if not set.
* See the source code of the script for more.

### Example build command

```shell
docker build -t generate-test-genesis .
```

### Example run command

```shell
docker run -e NUM_BAKERS=5 -e EXTRA_ACCOUNTS_TEMPLATE=extra -e NUM_EXTRA_ACCOUNTS=10 -e EXTRA_ACCOUNTS_BALANCE=1000000000000 -v "$PWD:/work" generate-test-genesis
```

The files are created with owner `root` so one might want to update their ownership:

```shell
chown -R <id>:<group> ./out
```

Remember to also include `genesis.json` when persisting the generated data, as that file is not copied into `./out`.
