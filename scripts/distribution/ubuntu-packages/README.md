# Building .deb packages for ubuntu distributions.

This directory contains instructions for building ubuntu packages with the node
and the collector included, and set up to run and be controlled via `systemd`.
In particular the packages will include
- genesis block for the network the packages are designed for
- the `concordium-node` binary
- the `node-collector` binary to report to the network dashboard if so desired.
- the start script for both services
- configuration files

The start scripts query for some information during setup, in particular they
query for the node name.

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

- [template/scripts/concordium-node-wrapper](template/scripts/concordium-node-wrapper)
  is the startup script covering both services. This should eventually be
  changed so that the binaries get their parameters directly from environment
  variables, making the script obsolete.

The strategy for building the package is as follows.

1. Obtain genesis-data for the specific network. Copy `genesis.dat` to the
   [template/data](template/data) directory (creating the directory as well).
2. Update [template/debian/changelog](template/debian/changelog) as appropriate.
3. Update [template/debian/control](template/debian/control) as appropriate.
4. Update the
   [template/debian/concordium-node.install](template/debian/concordium-node.install)
   file with the genesis hash (NB: This is __not__ the hash of genesis.dat file,
   it is the hash of the genesis block, which is defined differently. It is
   emitted by the genesis tool into a file `genesis_hash`)
5. Update
   [template/debian/concordium-node.service](template/debian/concordium-node.service)
   to match `CONFIG_DIR` and `DATA_DIR` to the genesis hash.
6. Possibly update  `BOOTSTRAP_FIRST_NODE` if it does not point to the correct
   network.
7. Possibly update `COLLECTOR_URL` in
   [template/debian/concordium-node-collector.service](template/debian/concordium-node-collector.service)
   to point to the correct backend for the network.
6. Obtain the `concordium-node` and `node-collector` binaries built for the
   specific ubuntu platform.

   The [template/debian/rules](template/debian/rules)
   script assumes that the `static-node-binaries` docker image is available (see
   [../static-binaries/README.md](../static-binaries/README.md) for details on
   how to obtain it). Make sure to use the `collector` feature to build them,
   e.g.,
   ```console
   $ UBUNTU_VERSION=20.04 STATIC_LIBRARIES_IMAGE_TAG=0.19 GHC_VERSION=8.10.4 EXTRA_FEATURES=collector ./scripts/static-binaries/build-static-binaries.sh
   ```

   If not then just comment out the relevant lines and manually copy the
   binaries into the [template/binaries](template/binaries) (creating the
   directory if needed).
6. Run `dpkg-buildpackage -us -uc --build=binary` from **inside the template directory**.
   This produces a .deb file in this directory. The file is named in line with
   the debian naming convention, including the version number.


# Installing the package

The .deb file can be installed in the usual way, i.e.,
```console
apt install ./$PACKAGE.deb
```
(the `./` is important, else `apt` will assume you want to install a package
from the registry)

This will take care to install dependencies first.

# Installed package configuration

The layout of the files in the package is as follows (for an example genesis).

```
.
├── lib
│   └── systemd
│       └── system
│           ├── concordium-node-collector.service
│           └── concordium-node.service
├── usr
│   ├── bin
│   │   ├── concordium-node
│   │   ├── concordium-node-wrapper
│   │   └── node-collector
│   └── share
│       └── doc
│           └── concordium-node
│               └── changelog.Debian.gz
└── var
    └── lib
        └── concordium
            └── b6078154d6717e909ce0da4a45a25151b592824f31624b755900a74429e3073d
                └── data
                    └── genesis.dat
```

The packaging achieves the following
- upon install the `concordium-node` and `concordium-node-collector` services will be started.
- removing the package will stop the services and delete all of the mentioned
  files.

In addition to the files above the following files are created by the
installation scripts

- `/etc/systemd/system/concordium-node-collector.service.d/override.conf`
  which is the configuration file with user-specific values for some environment
  variables. This currently contains only the node name that was chosen during
  installation.

These files are __not__ removed by the uninstall scripts since they contain
"user configuration". They should be removed manually if so desired.

# Configuration of the node

The node is configured via `systemd` unit files. When installed there is only
the system unit file in `/lib/systemd/system/concordium-node.service` which
contains reasonable defaults for an average system. However there are a number
of configuration options that could make sense to alter. The intended way to
modify node settings is to add `systemd` drop-in files. The simplest way to do
that is to use `systemctl edit`. To edit the node settings

```console
sudo systemctl edit concordium-node.service
```

this will open an editor which can be used to override the settings in
`/lib/systemd/system/concordium-node.service` and add new configuration options.
The first time this command is invoked a fresh file will be created at
`/etc/systemd/system/concordium-node.service.d/override.conf`. After that the
same file will be opened for editing.

The configuration should be done via `Environment` directives in a `[Service]`
section, e.g., an example file could be
```
[Service]
Environment=LISTEN_PORT=8888
```
which will set the environment variable `LISTEN_PORT` to `8888` for the node.

The node supports the following environment variables.

- `CONCORDIUM_NODE_BAKER_CREDENTIALS_FILE` the file with baker keys. This must be an absolute
  path. If it is set then the node will start as a baker. Setting this variable
  is the way for the node to become a baker.

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

- `EXTRA_ARGS` are any extra arguments that are not directly supported by
  environment variables. The complete list can be obtained by running
  `concordium-node --help`.

- `CONCORDIUM_NODE_RPC_SERVER_ADDR` is the listen address of the node's grpc server (default 0.0.0.0)

- `CONCORDIUM_NODE_RPC_SERVER_PORT` is the port of the grpc server (default 10000) (NB: If this
  is changed then the variable `COLLECTOR_GRPC_PORT` must be changed as well for
  the `concordium-node-collector` service)

- `CONCORDIUM_NODE_EXTERNAl_PORT` is related to the listen-port. If the external port of the
  server is not the same as the port the node is listening on (i.e., it is
  remapped) then this should be set to the external port so that other nodes can
  successfully connect.

- `TRUSTED_NODES_FILE` should be a path to a file. The file must be a list of
  lines, each line being in the format `addr:port` where addr and port are the
  address and port of a "trusted node". These are peers to which this node will
  try to keep connection to and never drop.

  NB: The node is sandboxed, so it would be best to put the file under the
  node's state directory `/var/lib/concordium/`

After editing the configuration file the node must be restarted. This can be
done via

```console
sudo systemctl restart concordium-node.service
```

## Caveat

If you edited the file in some other way, without using `systemctl edit` then
you need to first reload the configuration files. This can be done with

```console
sudo systemctl daemon-reload
```

## Monitoring

To see whether the node is running correctly you may use `systemctl` commands.

```console
sudo systemctl status concordium-node.service
```

will show whether the node is up and what configuration files it is using.

To see the contents of the configuration files you can use

```console
sudo systemctl cat concordium-node.service
```

The node logs data using `journald`. The logs can be obtained via `journalctl`,
e.g.,

```console
sudo journalctl -u concordium-node.service
```
or only the most recent (e.g., last 10min)

```console
sudo journalctl -u concordium-node.service --since '10min ago'
```

## Configuration of the collector

The main configuration option for the collector is the node name that appears on
the network dashboard. Use

```console
sudo systemctl edit concordium-node-collector.service
```
to edit. The environment variable name is `CONCORDIUM_NODE_COLLECTOR_NODE_NAME`.
