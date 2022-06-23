# The ubuntu version to build the package in. This influences the dependencies
# that will be added to the package. This should be the same as was used to
# build the binaries.
ARG ubuntu_version

FROM static-node-binaries as binaries

COPY template /pkg-root

RUN mkdir -p /pkg-root/binaries
RUN cp /build/bin/concordium-node /pkg-root/binaries/concordium-node
RUN cp /build/bin/node-collector /pkg-root/binaries/concordium-node-collector

FROM ubuntu:$ubuntu_version

COPY --from=binaries /pkg-root /pkg-root

WORKDIR /pkg-root

RUN apt-get update && \
DEBIAN_FRONTEND=noninteractive apt-get -y install debhelper dh-exec libgmp10

ARG build_env_name=Testnet
ARG build_env_name_lower=testnet
ARG build_genesis_hash=b6078154d6717e909ce0da4a45a25151b592824f31624b755900a74429e3073d
ARG build_collector_backend_url=https://dashboard.testnet.concordium.com/nodes/post
ARG build_rpc_server_port=10001
ARG build_listen_port=8889
ARG build_bootstrap=bootstrap.testnet.concordium.com:8888

RUN ./instantiate.sh
