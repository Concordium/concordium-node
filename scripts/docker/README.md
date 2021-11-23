# Dockerized components

The dockerfiles in this folder are used to build images for each of the components:

- `node`
- `bootstrapper`
- `node-collector`

All the components are compiled (in both release and debug) in a monolithic image from the dockerfile `universal.Dockerfile`.
The other dockerfiles just extract the individual binaries from this image, install dependencies, and declare exposed ports.

To run the node, the genesis file `genesis.dat` needs to be placed at the data path when the node starts.
The image built from `genesis.Dockerfile` contains just this file and a script for copying it into the correct path at load time.

## Jenkins Pipelines

The node-related binaries are built by the pipeline `master.Jenkinsfile` in the `jenkinsfiles` top-level folder.
Genesis images are built with `master-genesis.Jenkinsfile`.

## Docker Compose

The following example shows a minimal setup of a node and an accompanying collector:

```yaml
version: '3'
services:
  genesis:
    container_name: node-init-genesis
    image: ${GENESIS_IMAGE}
    entrypoint: cp /genesis.dat /data/genesis.dat
    network_mode: none
    volumes:
      - data:/data
  node:
    container_name: node
    image: ${NODE_IMAGE}
    depends_on:
      - genesis
    networks:
      - concordium
    environment:
      - CONCORDIUM_NODE_CONNECTION_BOOTSTRAP_NODES=bootstrap.${DOMAIN}:8888
      - CONCORDIUM_NODE_DATA_DIR=/var/lib/concordium/data
      - CONCORDIUM_NODE_CONFIG_DIR=/var/lib/concordium/config
      - CONCORDIUM_NODE_PROMETHEUS_SERVER=1
      - CONCORDIUM_NODE_RPC_SERVER_ADDR=0.0.0.0
    ports:
      - "8888:8888"
      - "10000:10000"
    volumes:
      - data:/var/lib/concordium/data
      - config:/var/lib/concordium/config
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
export GENESIS_IMAGE="192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/genesis:<genesis-tag>"
export NODE_IMAGE="192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/node:<tag>"
export NODE_COLLECTOR_IMAGE="192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/node-collector:<tag>"
export DOMAIN=mainnet.concordium.software
export NODE_NAME="<name>"
docker-compose up
```
