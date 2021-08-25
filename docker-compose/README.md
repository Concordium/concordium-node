# Development image for local deployment

This folder contains a `Dockerfile` for building a multi-component image that is well suited for
spinning up a network using docker-compose for local development.

## Usage

A parameterized Docker Compose file `bakers.yaml` is available:
It sets up a network of 1, 5, 10, or 25 bakers with collectors and a single collector-backend.

Expected environment variables:

- `NUM_BAKERS`: The number of bakers to start. Should be provided as argument to `--scale baker=` as well.
- `DESIRED_PEERS`: The number of peers expected by a baker node. Should be one less than `NUM_BAKERS`.

The following command will start a cluster of `<n>` (1, 5, 10, or 25) nodes:

```
NUM_BAKERS=<n> DESIRED_PEERS=<n-1> docker-compose -f bakers.yaml up --scale baker=<n>
```

Example: Boot a cluster of 5 nodes (with no reuse of containers):

```
NUM_BAKERS=5 DESIRED_PEERS=4 docker-compose -f bakers.yaml up --scale baker=5 --force-recreate
```

Update the instantiated Docker image using

```
docker-compose -f bakers.yaml pull
```

There is an intent to integrate a Wallet Proxy and a Middleware instance into the setup
once time permits...
This has been implemented in the past but was removed due to lack of maintenance.

The environment variables with prefix `CONCORDIUM_NODE_` defined in the Compose file are passed directly
to the relevant binary.
The full set of supported variables is listed in [VARIABLES.md](../VARIABLES.md).

## Accounts

The images come with accounts with private keys for the accounts that are
defined in genesis. This includes baker accounts. These keys are located in
`/genesis-data/genesis-$NUM_BAKERS-bakers/` directories. The baker accounts are
under the `bakers` subdirectory, and any additional accounts are under the
`accounts` subdirectory. They can be copied out either via `docker cp` when a
container is running, or via `docker run` and mapping /genesis-data to a host directory.

## Build

See [`dev-node.Jenkinsfile`](https://gitlab.com/Concordium/concordium-node/-/blob/master/jenkinsfiles/dev-node.Jenkinsfile).

# OLD

Information from the old README that might still be(come) relevant:
 
> Remember to clean out PostgreSQL data between runs using
> ```bash
> $ NUM_BAKERS=5 DESIRED_PEERS=4 docker-compose -f develop.wallet-dev.yml down
> ```
> 
> ## Delay baker startup if PostgreSQL starts too slowly
> If PostgreSQL starts too slowly the baker enabled for logging to it can be delayed by using the variable `DB_SLEEP` (the wallet-proxy has a default value of 30 set to delay start until PostgreSQL is up).
