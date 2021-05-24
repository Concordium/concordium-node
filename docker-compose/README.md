# Development image for local deployment

This folder contains a `Dockerfile` for building a multi-component image that is well suited for
spinning up a network using docker-compose for local development.

## Usage

Two parameterized `docker-compose` files are available:

### `bakers.yaml`

Run a network of bakers with collectors and a collector-backend.
This is useful for e.g. testing the network dashboard.
A middleware instance needs to be started separately.

#### Accounts

The images come with accounts with private keys for the accounts that are
defined in genesis. This includes baker accounts. These keys are located in
`/genesis-data/genesis-$NUM_BAKERS-bakers/` directories. The baker accounts are
under the `bakers` subdirectory, and any additional accounts are under the
`accounts` subdirectory. They can be copied out either via `docker cp` when a
container is running, or via `docker run` and mapping /genesis-data to a host directory.

#### Example

To boot a cluster of 5 nodes (no wallet-proxy), use the command

```
NUM_BAKERS=5 DESIRED_PEERS=4 docker-compose -f bakers.yaml up --scale baker=5 --force-recreate
```

Update the used docker image using 

```
docker-compose -f bakers.yaml pull
```
...


## The `baker+wallet-proxy+identity-provider.yaml` configuration

This is a compose file that starts a single baker with transaction logging, a
wallet proxy, an identity provider and an identity verifier. This
configuration persists state and assumes that there is a $HOST_ROOT directory
with the following layout present.
```
$HOST_ROOT
├── baker-reporter
│   ├── baker-0-credentials.json
│   └── genesis.dat
└── configuration
    ├── identity-provider
    │   ├── anonymity_revokers.json
    │   ├── global.json
    │   └── identity_provider.json
    ├── identity-verifier
    │   └── identity_provider.pub.json
    └── wallet-proxy
        ├── gtu-drop-account.json
        └── ips-with-metadata.json
```
The files must be mutually consistent
- `identity_provider.json` and `identity_provider.pub.json` must be private and
  public keys of the identity provider, and they must match the identity
  provider in `genesis.dat`.
- `gtu-drop-account.json` are private keys of an account in genesis.dat.
- `global.json` must be the cryptographic parameters matching `genesis.dat`.

This configuration additionally starts a postgres instance. The configuration
exposes the following services and ports
- a node GRPC API on port 10000
- collector backend on port 12000
- wallet-proxy on port 14000
- postgres database on 5432
- identity provider on port 7011
- identity verifier on port 7012

If any of these ports clash with existing services on the machine they should
be changed in the .yaml file.

To use this configuration first construct a $HOST_ROOT directory with at least
the contents listed above. NB: the value of the $HOST_ROOT environment
variable must be an absolute path **or a path relative to the .yaml file**.
Assuming you are in the root of the repository run. Otherwise paths to the
`.yaml` file must be updated.

```console
HOST_ROOT=$(pwd)/example docker-compose -f docker-compose/baker+wallet-proxy+identity-provider.yaml up --force-recreate
```
or similar, depending on what your HOST_ROOT is. The easiest is if you create
a directory at the root of the repository for HOST_ROOT.

### Generate data.

To generate suitable genesis configurations you can use the
[../scripts/genesis/generate-test-genesis.py](../scripts/genesis/generate-test-genesis.py)
script. A suitable example configuration can be generated with the following
invocaction (from the [../scripts/genesis/](../scripts/genesis/)) directory

```console
USE_DOCKER= PURGE= GENESIS_TIME=current NUM_IPS=1 NUM_ARS=3 EXTRA_ACCOUNTS_TEMPLATE=drop NUM_EXTRA_ACCOUNTS=1 EXTRA_ACCOUNTS_BALANCE=3500000.0 ./generate-test-genesis.py
```
this will generate 1 baker, 1 identity provider, 3 anonymity revokers, and an
additional "gtu drop account". They will all be in the `genesis_data` directory
or its subdirectory. For further options see the python script and its documentation.


## Build

See the [./Dockerfile](./Dockerfile) for details.

# OLD

Information from the old README that might still be relevant:
 
> Remember to clean out PostgreSQL data between runs using
> ```bash
> $ NUM_BAKERS=5 DESIRED_PEERS=4 docker-compose -f develop.wallet-dev.yml down
> ```
> 
> ## Delay baker startup if PostgreSQL starts too slowly
> If PostgreSQL starts too slowly the baker enabled for logging to it can be delayed by using the variable `DB_SLEEP` (the wallet-proxy has a default value of 30 set to delay start until PostgreSQL is up).
