# Building .deb packages for ubuntu distributions.

This directory contains instructions for building ubuntu packages with the node
and the collector included, and set up to run and be controlled via `systemd`.
In particular the packages will include
- genesis block for the network the packages are designed for
- the `concordium-node` binary
- the `node-collector` binary to report to the network dashboard if so desired.
- the start script for both services
- configuration files

The installation scripts query for some information during setup, in particular
they query for the node name.

The template for building the packages is in the [template](./template)
directory. The following files are relevant

- [template/debian/changelog](template/debian/changelog) the changelog for the package that
  should adhere to the debian maintainer policy. Upon each release it should be
  updated with appropriate information.

- [template/debian/concordium-node.config](template/debian/concordium-node.config) the
  `debconf` configuration file to query relevant parameters during package
  installation. This file goes together with [template/debian/concordium-node.templates](template/debian/concordium-node.templates).

- [template/debian/concordium-node.templates](template/debian/concordium-node.templates) These
  are templates for the queries of parameters. This should be updated as
  appropriate to, e.g., update default values or add additional parameters.

- [template/debian/concordium-node.service](template/debian/concordium-node.service) is the
  systemd unit file with the default values of parameters and start arguments.

- [template/debian/concordium-node-collector.service](template/debian/concordium-node-collector.service) is the
  systemd unit file with the default values of parameters for the
  `node-collector` service.

- [template/debian/concordium-node.install](template/debian/concordium-node.install) lists
  additional files that should be installed. This includes the binaries and the
  startup script.

- [template/debian/control](template/debian/control) is the package metadata.

- [template/debian/postinst](template/debian/postinst) is the post installation
  script of the package. We use it to set up the values of parameters that were
  queried from the user.

- [template/debian/rules](template/debian/rules) is a Makefile that is used by
  the `debhelper` tool to generate the package. We use a mostly default file,
  with minor modifications to install two systemd services as opposed to a
  single, which would be the default.

These files contain a number of placeholders that can be instantiated when
building a package for different environments. These are meant to be
instantiated during build time with a tool such as `envsubst`. The variables are

- `build_version` (e.g., 1.1.0, should match the concordium-node version)
- `build_env_name` (e.g., Testnet)
- `build_env_name_lower` (e.g., testnet)
- `build_genesis_hash` (hash of the genesis block (NB: This is not the same
  as the hash of the genesis.dat file, instead it is the protocol defined hash of the contents of the genesis block.))
- `build_collector_backend_url` (e.g. https://dashboard.testnet.concordium.com/nodes/post)
- `build_rpc_server_port` (e.g., 10001)
- `build_grpc2_listen_port` (e.g., 20001)
- `build_listen_port` (e.g., 8889)
- `build_bootstrap` (e.g., bootstrap.testnet.concordium.com:8888)

There is a script [./template/instantiate.sh](./template/instantiate.sh) to
instantiate the template to obtain a concrete package. See the script
documentation for the steps needed to instantiate the template.

The general strategy for building the package is as follows.

1. Obtain genesis-data for the specific network. Copy `genesis.dat` to the
   [template/data](template/data) directory, naming it
   `${build_env_name_lower}-genesis.dat` (e.g., `testnet-genesis.dat`).
2. Update [template/debian/changelog](template/debian/changelog) as appropriate.
3. Update [template/debian/control](template/debian/control) as appropriate.
4. Possibly update `CONCORDIUM_NODE_COLLECTOR_URL` in
   [template/debian/concordium-node-collector.service](template/debian/concordium-node-collector.service)
   to point to the correct backend for the network.
5. Obtain the `concordium-node` and `node-collector` binaries built for the
   specific ubuntu platform.

   For example use the [../../static-binaries/build-static-binaries.sh](../../static-binaries/build-static-binaries.sh).
   ```console
   $ UBUNTU_VERSION=20.04 STATIC_LIBRARIES_IMAGE_TAG=ghc-9.2.5 
        STATIC_BINARIES_IMAGE_TAG=latest GHC_VERSION=9.2.5 EXTRA_FEATURES=collector ./scripts/static-binaries/build-static-binaries.sh
   ```

   If not then just comment out the relevant lines and manually copy the
   binaries into the [template/binaries](template/binaries) (creating the
   directory if needed).
6. Run [template/instantiate.sh](./template/instantiate.sh) script from **inside the template directory**.
   This produces a .deb file in this directory. The file is named in line with
   the debian naming convention, including the version number.


# Mainnet and testnet builds

Since mainnet and testnet builds are common the repository includes two scripts
that will build packages for those two environments with minimal input. These
scripts are [./build-mainnet-deb.sh](./build-mainnet-deb.sh) and
[./build-testnet-deb.sh](./build-testnet-deb.sh). They should be run from the
directory they are in as follows.

```console
UBUNTU_VERSION=20.04 ./build-testnet-deb.sh
```

The script uses docker to build the binaries and the debian package. The output
of the script will be in the directory `testnet-build` (or `mainnet-build`).

# Installing the package

The .deb file can be installed in the usual way, i.e.,
```console
apt install ./$PACKAGE.deb
```
(the `./` is important, else `apt` will assume you want to install a package
from the registry)

This will take care to install dependencies first.

# Installed package configuration

The layout of the files in the package is as follows (for an example genesis)
and the `testnet` environment.

```
.
├── lib
│   └── systemd
│       └── system
│           ├── concordium-testnet-node-collector.service
│           └── concordium-testnet-node.service
├── usr
│   ├── bin
│   │   ├── concordium-testnet-node-1.1.0
│   │   └── concordium-testnet-node-collector-1.1.0
│   └── share
│       └── doc
│           └── concordium-testnet-node
│               └── changelog.Debian.gz
└── var
    └── lib
        └── concordium-b6078154d6717e909ce0da4a45a25151b592824f31624b755900a74429e3073d
            └── data
                └── genesis.dat
```

The packaging achieves the following
- upon install the `concordium-testnet-node` and `concordium-testnet-node-collector` services will be started.
- removing the package will stop the services and delete all of the mentioned
  files.

In addition to the files above the following files are created by the
installation scripts

- `/etc/systemd/system/concordium-testnet-node-collector.service.d/override.conf`
  which is the configuration file with user-specific values for some environment
  variables. This currently contains only the node name that was chosen during
  installation.

These files are __not__ removed by the uninstall scripts since they contain
"user configuration". They should be removed manually if so desired.

# Configuration of the node

The node is configured via `systemd` unit files. When installed there is only
the system unit file in `/lib/systemd/system/concordium-${build_env_name_lower}-node.service` which
contains reasonable defaults for an average system. However there are a number
of configuration options that could make sense to alter. The intended way to
modify node settings is to add `systemd` drop-in files. The simplest way to do
that is to use `systemctl edit`. To edit the node settings

```console
sudo systemctl edit concordium-${build_env_name_lower}-node.service
```

this will open an editor which can be used to override the settings in
`/lib/systemd/system/concordium-${build_env_name_lower}-node.service` and add new configuration options.
The first time this command is invoked a fresh file will be created at
`/etc/systemd/system/concordium-${build_env_name_lower}-node.service.d/override.conf`. After that the
same file will be opened for editing.

The configuration should be done via `Environment` directives in a `[Service]`
section, e.g., an example file could be
```
[Service]
Environment=CONCORDIUM_NODE_LISTEN_PORT=8888
```
which will set the environment variable `CONCORDIUM_NODE_LISTEN_PORT` to `8888` for the node.

The node supports the following environment variables.

- `CONCORDIUM_NODE_BAKER_CREDENTIALS_FILE` the file with baker keys.
  If it is set then the node will start as a baker, or at least attempt to.
  This must be a path relative to the `WorkingDirectory` or an absolute path.
  Since the node is sandboxed it does not have access to the `/home` directory
  and some other parts of the system.
  The recommended way to expose the baker keys to the node is to use the
  `BindReadOnlyPaths` option to remap the file from wherever it is on the host
  system to a location which the node can read. For example (this assumes the baker keys are located in `/home/user/baker-credentials.json` on the host system)
  ```
  Environment=CONCORDIUM_NODE_BAKER_CREDENTIALS_FILE=%S/concordium-${build_genesis_hash}/baker-credentials.json
  BindReadOnlyPaths=/home/user/baker-credentials.json:%S/concordium-${build_genesis_hash}/baker-credentials.json
  ```


- `CONCORDIUM_NODE_LISTEN_PORT`, the port on which the node is listening for incoming
  connections. This should be opened in any firewall rules so that the node is a
  good network participant. Default value is 8888

- `BOOTSTRAP_FIRST_NODE` is the address:port of the bootstrap node. This is the
  first node to connect to. Generally this should be left at the default value.

- `CONCORDIUM_NODE_CONNECTION_DESIRED_NODES` is the minimum number of peers the node will want to have. If
  it has fewer than the given number it will attempt to acquire more via
  bootstraping or asking its existing peers for their peers.

- `CONCORDIUM_NODE_CONNECTION_MAX_ALLOWED_NODES` is the maximum number of peers the node will tolerate. If
  it gets more it will drop peers until the number drops below.

- `CONCORDIUM_NODE_CONNECTION_HARD_CONNECTION_LIMIT` is the maximum number of **connections** (as opposed
  to nodes) that a node will have at a given time. This should be set a bit
  higher than the maximum number of nodes so that new peers are accepted and
  discovered over time. Default value is 20.

- `CONCORDIUM_NODE_CONNECTION_THREAD_POOL_SIZE` is the number of threads the node should use for processing
  network messages. This should be fewer than the available number of hardware
  threads to allow consensus the space to work. Default value is `2`.

- `CONCORDIUM_NODE_CONFIG_DIR`, `CONCORDIUM_NODE_DATA_DIR` are directories where the node stores its
  configuration and data. In particular the `CONCORDIUM_NODE_DATA_DIR` is where the node's
  database is stored.

- `CONCORDIUM_NODE_RPC_SERVER_ADDR` is the listen address of the node's grpc server (default 0.0.0.0)

- `CONCORDIUM_NODE_RPC_SERVER_PORT` is the port of the grpc server (default 10000) (NB: If this
  is changed then the variable `COLLECTOR_GRPC_PORT` must be changed as well for
  the `concordium-${build_env_name_lower}-node-collector` service)

- `CONCORDIUM_NODE_EXTERNAL_PORT` is related to the listen-port. If the external port of the
  server is not the same as the port the node is listening on (i.e., it is
  remapped) then this should be set to the external port so that other nodes can
  successfully connect.

- `CONCORDIUM_NODE_CONNECTION_CONNECT_TO` is a comma separated list of peers which the node will try
to keep connection to and never drop. The peers must be in the format `addr:port` where addr and port are the
  address and port of a "trusted node".

- The complete list of configuration options can be obtained by running
  `concordium-${build_env_name_lower}-node --help`.

After editing the configuration file the node must be restarted. This can be
done via

```console
sudo systemctl restart concordium-${build_env_name_lower}-node.service
```

## Caveat

If you edited the file in some other way, without using `systemctl edit` then
you need to first reload the configuration files. This can be done with

```console
sudo systemctl daemon-reload
```

## Out of band catchup

The debian package can be configured to import blocks from a local file to speed up initial catchup.
To configure a node for out of band catchup do the following.

1. Save the file with blocks to import to `$BLOCKS_TO_IMPORT`.
2. If the node is running stop it.
```console
sudo systemctl stop concordium-${build_env_name_lower}-node.service
```
3. Edit the node's configuration file
```console
sudo systemctl edit concordium-${build_env_name_lower}-node.service
```
In the `[Service]` section add
```
Environment=CONCORDIUM_NODE_CONSENSUS_IMPORT_BLOCKS_FROM=%S/concordium-${build_genesis_hash}/blocks_to_import.dat
BindReadOnlyPaths=$BLOCKS_TO_IMPORT:%S/concordium-${build_genesis_hash}/blocks_to_import.dat
```
replacing `$BLOCKS_TO_IMPORT` with the path to the downloaded file.
4. Start the node again.
```console
sudo systemctl start concordium-${build_env_name_lower}-node.service
```

After the block import is completed we recommend to edit the configuration file again removing
```
Environment=CONCORDIUM_NODE_CONSENSUS_IMPORT_BLOCKS_FROM=%S/concordium-${build_genesis_hash}/blocks_to_import.dat
BindReadOnlyPaths=$BLOCKS_TO_IMPORT:%S/concordium-${build_genesis_hash}/blocks_to_import.dat
```
so that in subsequent restarts out of band catchup will be disabled, which will speed up restarts.

## Maintenance

To see whether the node is running correctly you may use `systemctl` commands.

```console
sudo systemctl status concordium-${build_env_name_lower}-node.service
```

will show whether the node is up and what configuration files it is using.

To see the contents of the configuration files you can use

```console
sudo systemctl cat concordium-${build_env_name_lower}-node.service
```

The node logs data using `journald`. The logs can be obtained via `journalctl`,
e.g.,

```console
sudo journalctl -u concordium-${build_env_name_lower}-node.service
```
or only the most recent (e.g., last 10min)

```console
sudo journalctl -u concordium-${build_env_name_lower}-node.service --since '10min ago'
```

### Troubleshooting
It can happen that the database of the concordium-node gets corrupted by a sudden shutdown of the concordium-node service e.g., by a sudden machine shutdown etc.

This will result in an endless loop of service restarts as the concordium-node cannot startup successfully due to a database corruption.
If this is the case then the log of the concordium-node will contain errors along the lines:

```
error, called at src/Concordium/GlobalState/Persistent/BlobStore.hs
...
concordium-${build_env_name_lower}-node: warning: too many hs_exit()s
concordium-${build_env_name_lower}-node.service: Main process exited, code=exited, status=1/FAILURE
concordium-${build_env_name_lower}-node.service: Failed with result 'exit-code'.
```

In order to `recover` from such a situation, one should take the following steps.

- Stop the concordium-${build_env_name_lower}-node service by running the command `sudo systemctl stop concordium-${build_env_name_lower}-node.service`

- Delete the contents (**except** the genesis.dat file) of the `data` folder.
The default path for the `data` folder is `/var/lib/concordium/$GENESIS_HASH/data/`.
Where $GENESIS_HASH is a hash derived from the configured genesis data.

- Start the concordium-${build_env_name_lower}-node service again by running `sudo systemctl start concordium-${build_env_name_lower}-node-collector.service`.

The concordium-node should now be up and running again.
Verify with the command: `sudo systemctl status concordium-${build_env_name_lower}-node`.

## Configuration of the collector

The main configuration option for the collector is the node name that appears on
the network dashboard. Use

```console
sudo systemctl edit concordium-${build_env_name_lower}-node-collector.service
```
to edit. The environment variable name is `CONCORDIUM_NODE_COLLECTOR_NODE_NAME`.
