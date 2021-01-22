# syntax=docker/dockerfile:experimental
FROM 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/base:0.17 as build

ARG consensus_type
ENV CONSENSUS_TYPE=$consensus_type
ARG consensus_profiling
ENV CONSENSUS_PROFILING=$consensus_profiling

COPY . /build-project/
COPY scripts/start.sh /build-project/start.sh
COPY scripts/build-binaries.sh /build-project/build-binaries.sh

WORKDIR /build-project

RUN --mount=type=ssh ./scripts/download-static-libs.sh

### Baker id gen
RUN rustup install nightly-2020-06-10 && \
    cd baker_id_gen && \
    cargo +nightly-2020-06-10 build --release && \
    mv target/release/baker_id_gen ../baker_id_generator && \
    cd .. && \
    rm -rf baker_id_gen

### P2P client
RUN --mount=type=ssh ./build-binaries.sh "collector"

RUN chmod +x /build-project/start.sh

## Wallet-proxy
FROM 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/base-haskell:0.17 as wallet-proxy-build
WORKDIR /
ENV STACK_ROOT /.stack
RUN mkdir -p -m 0600 ~/.ssh && ssh-keyscan gitlab.com >> ~/.ssh/known_hosts
RUN --mount=type=ssh git clone --recurse-submodules --depth 1 --branch master git@gitlab.com:Concordium/tools/wallet-proxy.git
WORKDIR /wallet-proxy
RUN stack build --copy-bins --ghc-options -j4 --local-bin-path target
RUN mkdir -p /bins
RUN cp target/wallet-proxy-exe /bins/wallet-proxy

FROM ubuntu:20.04

EXPOSE 8950
EXPOSE 8888
EXPOSE 9090
EXPOSE 8900
EXPOSE 10000

RUN apt-get update && apt-get install -y unbound curl postgresql-server-dev-12 libnuma1

COPY --from=build /build-project/baker_id_generator /baker_id_generator
COPY --from=build /build-project/start.sh /start.sh
COPY --from=build /build-project/genesis-data /genesis-data
COPY --from=build /build-project/genesis-complementary-bundle /genesis-complementary-bundle
COPY --from=build /build-project/concordium-node/target/debug/concordium-node /concordium-node
COPY --from=build /build-project/concordium-node/target/debug/p2p_bootstrapper-cli /p2p_bootstrapper-cli
COPY --from=build /build-project/concordium-node/target/debug/node-collector /node-collector
COPY --from=build /build-project/concordium-node/target/debug/node-collector-backend /node-collector-backend 
COPY --from=wallet-proxy-build /wallet-proxy/target/wallet-proxy-exe /wallet-proxy
COPY --from=wallet-proxy-build /wallet-proxy/deps/simple-client/deps/crypto/rust-src/target/release/*.so /usr/lib/

ENTRYPOINT ["/start.sh"]
