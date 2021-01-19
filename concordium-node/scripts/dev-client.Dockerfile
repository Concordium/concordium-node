# syntax=docker/dockerfile:experimental
FROM 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/base:0.15 as build

ARG consensus_type
ENV CONSENSUS_TYPE=$consensus_type
ARG consensus_profiling
ENV CONSENSUS_PROFILING=$consensus_profiling

COPY . /build-project/
WORKDIR /build-project
COPY scripts/start.sh ./start.sh
COPY scripts/init.build.env.sh ./init.build.env.sh
COPY scripts/build-binaries.sh ./build-binaries.sh
COPY ./genesis-data ./genesis-data

# Build Environment: Hacl, ffi, Haskell (inherited from k8 build)
RUN --mount=type=ssh ./init.build.env.sh

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

RUN cp /build-project/target/debug/concordium-node /build-project/target/debug/p2p_bootstrapper-cli /build-project/target/debug/node-collector /build-project/target/debug/node-collector-backend /build-project/

## Wallet-proxy
FROM 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/base-haskell:0.12 as wallet-proxy-build
WORKDIR /
ENV STACK_ROOT /.stack
RUN mkdir -p -m 0600 ~/.ssh && ssh-keyscan gitlab.com >> ~/.ssh/known_hosts
RUN --mount=type=ssh git clone --recurse-submodules --depth 1 --branch master git@gitlab.com:Concordium/tools/wallet-proxy.git
WORKDIR /wallet-proxy
RUN ( cd deps/simple-client && ./build-deps.sh )
RUN mkdir -p /libs
RUN cp deps/simple-client/extra-libs/*.so /libs
ENV LD_LIBRARY_PATH=/libs
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
COPY --from=build /build-project/concordium-node /concordium-node
COPY --from=build /build-project/p2p_bootstrapper-cli /p2p_bootstrapper-cli
COPY --from=build /build-project/node-collector /node-collector
COPY --from=build /build-project/node-collector-backend /node-collector-backend 
COPY --from=wallet-proxy-build /wallet-proxy/target/wallet-proxy-exe /wallet-proxy
COPY --from=wallet-proxy-build /libs/* /usr/lib/

ENTRYPOINT ["/start.sh"]
