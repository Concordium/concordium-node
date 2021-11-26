# Dockerized components

The dockerfiles in this folder are used to build images for each of the components:

- `node`
- `bootstrapper`
- `node-collector`

All the components are compiled (in both release and debug) in a monolithic image from the dockerfile `universal.Dockerfile`.
The other dockerfiles just extract the individual binaries from this image, install dependencies, and declare exposed ports.

To run the node, the genesis file `genesis.dat` needs to be placed at the data path when the node starts.
One way to do this is to inject the file as a bind mount as shown in the example below.

## Jenkins Pipelines

The node-related binaries are built by the pipeline `master.Jenkinsfile` in the `jenkinsfiles` top-level folder.
Genesis images are built with `master-genesis.Jenkinsfile`.

## Docker Compose

The following example shows a (reasonably) minimal setup of a node and an accompanying collector:

```yaml
version: '3'
services:
  node:
    container_name: node
    image: ${NODE_IMAGE}
    networks:
    - concordium
    environment:
    - CONCORDIUM_NODE_CONNECTION_BOOTSTRAP_NODES=bootstrap.${DOMAIN}:8888
    - CONCORDIUM_NODE_DATA_DIR=/mnt/data
    - CONCORDIUM_NODE_CONFIG_DIR=/mnt/config
    - CONCORDIUM_NODE_PROMETHEUS_SERVER=1
    - CONCORDIUM_NODE_RPC_SERVER_ADDR=0.0.0.0
    - CONCORDIUM_NODE_CONSENSUS_GENESIS_DATA_FILE=/mnt/genesis.dat
    ports:
    - "8888:8888"
    - "10000:10000"
    volumes:
    - ${GENESIS_DATA_FILE}:/mnt/genesis.dat
    - data:/mnt/data
    - config:/mnt/config
  node-collector:
    container_name: node-collector
    image: ${NODE_COLLECTOR_IMAGE}
    depends_on:
    - node
    networks:
    - concordium
    environment:
    - CONCORDIUM_NODE_COLLECTOR_URL=http://dashboard.${DOMAIN}/nodes/post
    - CONCORDIUM_NODE_COLLECTOR_GRPC_HOST=http://node:10000
    - CONCORDIUM_NODE_COLLECTOR_NODE_NAME=${NODE_NAME}
volumes:
  data:
  config:
networks:
  concordium:
```

Run the script using `docker-compose`; for example:

```shell
export NODE_IMAGE="192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/node:<tag>"
export NODE_COLLECTOR_IMAGE="192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/node-collector:<tag>"
export DOMAIN=mainnet.concordium.software # alternative values: 'stagenet.concordium.com', 'testnet.concordium.com'
export GENESIS_DATA_FILE="/absolute/path/to/genesis.dat"
export NODE_NAME="<name>"
docker-compose up
```
