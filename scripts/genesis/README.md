# Genesis configurations

The following genesis configurations are available in this directory.

## [genesis.json](./genesis.json)

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

### Example build command

```shell
docker build -t generate-test-genesis .
```

### Example run command

```shell
docker run -e NUM_BAKERS=5 -v "$PWD:/work" generate-test-genesis
```

The files are created with owner `root` so one might want to update their ownership:

```shell
chown -R <id>:<group> ./out
```
