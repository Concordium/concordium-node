# Concordium Node

This image runs a Concordium node and includes the `node-collector` process. It contains the mainnet and testnet genesis files. You must select a network and provide its network settings when you start the container.

It runs as the unprivileged user `10001:10001` and stores mutable state in `/mnt/data`.

The gRPC V2 API listens on `0.0.0.0:20000` inside the container by default. Docker does not make this port available on the host unless you publish it. Containers on the same Docker network can still reach the port.

## Run a mainnet node

Create a persistent Docker volume:

```shell
docker volume create concordium-mainnet-data
```

Start the node:

```shell
docker run --detach \
  --name concordium-mainnet-node \
  --restart unless-stopped \
  --publish 8888:8888 \
  --publish 20000:20000 \
  --mount type=volume,source=concordium-mainnet-data,target=/mnt/data \
  --env CONCORDIUM_NODE_CONSENSUS_GENESIS_DATA_FILE=/genesis/mainnet-genesis.dat \
  --env CONCORDIUM_NODE_CONNECTION_BOOTSTRAP_NODES=bootstrap.mainnet.concordium.software:8888 \
  --env CONCORDIUM_NODE_CONSENSUS_DOWNLOAD_BLOCKS_FROM=https://catchup.mainnet.concordium.software/blocks.idx \
  --env CONCORDIUM_NODE_COLLECTOR_ENABLED=true \
  --env CONCORDIUM_NODE_COLLECTOR_NODE_NAME=my-mainnet-node \
  --env CONCORDIUM_NODE_COLLECTOR_URL=https://dashboard.mainnet.concordium.software/nodes/post \
  concordium/node
```

Alternatively, create a `compose.yaml` file:

```yaml
services:
  node:
    image: concordium/node
    restart: unless-stopped
    ports:
      - "8888:8888"
      - "20000:20000"
    volumes:
      - concordium-mainnet-data:/mnt/data
    environment:
      CONCORDIUM_NODE_CONSENSUS_GENESIS_DATA_FILE: /genesis/mainnet-genesis.dat
      CONCORDIUM_NODE_CONNECTION_BOOTSTRAP_NODES: bootstrap.mainnet.concordium.software:8888
      CONCORDIUM_NODE_CONSENSUS_DOWNLOAD_BLOCKS_FROM: https://catchup.mainnet.concordium.software/blocks.idx
      CONCORDIUM_NODE_COLLECTOR_ENABLED: "true"
      CONCORDIUM_NODE_COLLECTOR_NODE_NAME: my-mainnet-node
      CONCORDIUM_NODE_COLLECTOR_URL: https://dashboard.mainnet.concordium.software/nodes/post

volumes:
  concordium-mainnet-data:
```

Start the service from the directory that contains `compose.yaml`:

```shell
docker compose up --detach
```

## Run a testnet node

Create a persistent Docker volume:

```shell
docker volume create concordium-testnet-data
```

Start the node:

```shell
docker run --detach \
  --name concordium-testnet-node \
  --restart unless-stopped \
  --publish 8889:8889 \
  --publish 20001:20000 \
  --mount type=volume,source=concordium-testnet-data,target=/mnt/data \
  --env CONCORDIUM_NODE_CONSENSUS_GENESIS_DATA_FILE=/genesis/testnet-genesis.dat \
  --env CONCORDIUM_NODE_CONNECTION_BOOTSTRAP_NODES=bootstrap.testnet.concordium.com:8888 \
  --env CONCORDIUM_NODE_CONSENSUS_DOWNLOAD_BLOCKS_FROM=https://catchup.testnet.concordium.com/blocks.idx \
  --env CONCORDIUM_NODE_LISTEN_PORT=8889 \
  --env CONCORDIUM_NODE_COLLECTOR_ENABLED=true \
  --env CONCORDIUM_NODE_COLLECTOR_NODE_NAME=my-testnet-node \
  --env CONCORDIUM_NODE_COLLECTOR_URL=https://dashboard.testnet.concordium.com/nodes/post \
  concordium/node
```

Alternatively, create a `compose.yaml` file:

```yaml
services:
  node:
    image: concordium/node
    restart: unless-stopped
    ports:
      - "8889:8889"
      - "20001:20000"
    volumes:
      - concordium-testnet-data:/mnt/data
    environment:
      CONCORDIUM_NODE_CONSENSUS_GENESIS_DATA_FILE: /genesis/testnet-genesis.dat
      CONCORDIUM_NODE_CONNECTION_BOOTSTRAP_NODES: bootstrap.testnet.concordium.com:8888
      CONCORDIUM_NODE_CONSENSUS_DOWNLOAD_BLOCKS_FROM: https://catchup.testnet.concordium.com/blocks.idx
      CONCORDIUM_NODE_LISTEN_PORT: "8889"
      CONCORDIUM_NODE_COLLECTOR_ENABLED: "true"
      CONCORDIUM_NODE_COLLECTOR_NODE_NAME: my-testnet-node
      CONCORDIUM_NODE_COLLECTOR_URL: https://dashboard.testnet.concordium.com/nodes/post

volumes:
  concordium-testnet-data:
```

Start the service from the directory that contains `compose.yaml`:

```shell
docker compose up --detach
```

## Run on a custom network

Provide a genesis file and the settings for the custom network. This example mounts `custom-genesis.dat` from the current directory:

```shell
docker volume create concordium-custom-data

docker run --detach \
  --name concordium-custom-node \
  --restart unless-stopped \
  --publish 8888:8888 \
  --publish 20000:20000 \
  --mount type=volume,source=concordium-custom-data,target=/mnt/data \
  --mount type=bind,source="$(pwd)/custom-genesis.dat",target=/genesis/custom-genesis.dat,readonly \
  --env CONCORDIUM_NODE_CONSENSUS_GENESIS_DATA_FILE=/genesis/custom-genesis.dat \
  --env CONCORDIUM_NODE_CONNECTION_BOOTSTRAP_NODES=bootstrap.custom.example:8888 \
  concordium/node
```

Alternatively, put `custom-genesis.dat` next to this `compose.yaml` file:

```yaml
services:
  node:
    image: concordium/node
    restart: unless-stopped
    ports:
      - "8888:8888"
      - "20000:20000"
    volumes:
      - concordium-custom-data:/mnt/data
      - ./custom-genesis.dat:/genesis/custom-genesis.dat:ro
    environment:
      CONCORDIUM_NODE_CONSENSUS_GENESIS_DATA_FILE: /genesis/custom-genesis.dat
      CONCORDIUM_NODE_CONNECTION_BOOTSTRAP_NODES: bootstrap.custom.example:8888

volumes:
  concordium-custom-data:
```

Start the service from the directory that contains both files:

```shell
docker compose up --detach
```

Omit `CONCORDIUM_NODE_CONSENSUS_DOWNLOAD_BLOCKS_FROM` when the custom network does not provide a catch-up index. Configure the bootstrap address, ports, and other node settings for that network. Replace the example collector URL with the backend URL for the custom network.

## Configure the node collector

The run and Compose examples enable the collector in the same container as the node. Replace each example node name with a unique name before you start the container. Use the collector backend URL for the selected network. Set `CONCORDIUM_NODE_COLLECTOR_ENABLED=false` to run the node without the collector.

The node continues to run if the collector exits.

## Monitor the node

The collector sends selected node information to the network dashboard. Prometheus metrics and gRPC health checks provide separate signals for your own monitoring system.

### Prometheus metrics

The Prometheus exporter is disabled by default. Set its listen address and port to enable it. Add these options to a `docker run` command:

```shell
--publish 127.0.0.1:9100:9100 \
--env CONCORDIUM_NODE_PROMETHEUS_LISTEN_ADDRESS=0.0.0.0 \
--env CONCORDIUM_NODE_PROMETHEUS_LISTEN_PORT=9100
```

For Docker Compose, add this configuration to the `node` service:

```yaml
services:
  node:
    ports:
      - "127.0.0.1:9100:9100"
    environment:
      CONCORDIUM_NODE_PROMETHEUS_LISTEN_ADDRESS: 0.0.0.0
      CONCORDIUM_NODE_PROMETHEUS_LISTEN_PORT: "9100"
```

Read the metrics from the host:

```shell
curl --fail http://127.0.0.1:9100/metrics
```

The exporter does not require authentication. Bind it to a trusted interface or protect it with your monitoring infrastructure. If Prometheus runs on the same Compose network, you do not need to publish the port to the host. Configure Prometheus to scrape `node:9100` and keep the exporter listen address set to `0.0.0.0`.

### gRPC health checks

The node provides the standard gRPC health service on the gRPC V2 port. The default container port is `20000`. For example, use `grpcurl` against a port that you published on the host:

```shell
grpcurl -plaintext -d '{}' 127.0.0.1:20000 grpc.health.v1.Health/Check
```

The health check verifies that consensus is running and that the last finalized block is recent. It also checks the validator committee status when the node is an active validator. Set `CONCORDIUM_NODE_GRPC2_HEALTH_MIN_PEERS` to require a minimum peer count. Set `CONCORDIUM_NODE_GRPC2_HEALTH_MAX_FINALIZED_DELAY` to change the maximum finalization delay from its default of 300 seconds.

## Persistent storage and permissions

The default data and configuration directory is:

```text
/mnt/data
```

Docker-managed volumes use the directory ownership from the image. If you use a host bind mount, make the directory writable by UID and GID `10001:10001`:

```shell
sudo chown -R 10001:10001 /path/to/concordium-data
```

Back up this directory according to your operating procedures.

## View logs and stop the node

```shell
docker logs --follow concordium-mainnet-node
docker stop concordium-mainnet-node
docker rm concordium-mainnet-node
```

Replace the container name when you run testnet or a custom network.

For Docker Compose, run these commands from the directory that contains `compose.yaml`:

```shell
docker compose logs --follow node
docker compose stop node
docker compose down
```

## Pass node arguments

Arguments that start with `-` are passed to `concordium-node`:

```shell
docker run --rm concordium/node --help
```

Run another image command by supplying its complete path:

```shell
docker run --rm concordium/node node-collector --help
```

With Docker Compose, use the configured `node` service:

```shell
docker compose run --rm node --help
docker compose run --rm node node-collector --help
```

## Migrate from the network-specific images

This image uses different filesystem paths and defaults than the previous `concordium/mainnet-node` and `concordium/testnet-node` images.

Update binary paths as follows:

```text
/concordium-node  -> /usr/local/bin/concordium-node
/node-collector   -> /usr/local/bin/node-collector
```

Both binaries are on `PATH`. Remove an explicit node `entrypoint` to use the image default, or update it to `/usr/local/bin/concordium-node`. Update a separate collector container to use `/usr/local/bin/node-collector`.

Update genesis paths as follows:

```text
/mainnet-genesis.dat -> /genesis/mainnet-genesis.dat
/testnet-genesis.dat -> /genesis/testnet-genesis.dat
```

The previous images run as root. This image runs as UID and GID `10001:10001`. Make each bind-mounted data directory writable by that identity before migration:

```shell
sudo chown -R 10001:10001 /path/to/concordium-data
```

The default data and configuration directory remains `/mnt/data`, as in the previous images. Existing volume and bind-mount targets can remain unchanged.

The combined image does not select a network automatically. Set the genesis file, bootstrap address, catch-up URL, and any network-specific P2P port. The examples below provide these values. Other node settings use their built-in defaults.
