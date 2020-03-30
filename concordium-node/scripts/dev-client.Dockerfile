# syntax=docker/dockerfile:experimental
FROM 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/base:0.11 as build

ARG consensus_type
ENV CONSENSUS_TYPE=$consensus_type
ARG consensus_profiling=true
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
RUN \
    rustup install nightly-2019-07-10 && \
    cd baker_id_gen && \
    cargo +nightly-2019-07-10 build --release && \
    mv target/release/baker_id_gen ../baker_id_generator && \
    cd .. && \
    rm -rf baker_id_gen

### Wallet-server
RUN \
    ( cd deps/internal/consensus/crypto/rust-bins && cargo build --release ) && \
    mv deps/internal/consensus/crypto/rust-bins/target/release/wallet_server .
    
### P2P client
RUN --mount=type=ssh ./build-binaries.sh "elastic_logging,collector"

RUN chmod +x /build-project/start.sh

RUN cp /build-project/target/debug/p2p_client-cli /build-project/target/debug/p2p_bootstrapper-cli /build-project/target/debug/node-collector /build-project/target/debug/node-collector-backend /build-project/

### Wallet-proxy
FROM 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/base-haskell:0.10 as wallet-proxy-build
WORKDIR /
RUN mkdir -p -m 0600 ~/.ssh && ssh-keyscan gitlab.com >> ~/.ssh/known_hosts
RUN --mount=type=ssh git clone --recurse-submodules git@gitlab.com:Concordium/tools/wallet-proxy.git
WORKDIR /wallet-proxy
RUN git checkout ab15e0cdade2c038f4c01977246359bd3a60b82e
RUN ( cd deps/simple-client && ./build-deps.sh )
RUN mkdir -p /libs
RUN cp deps/simple-client/extra-libs/*.so /libs
ENV LD_LIBRARY_PATH=/libs
RUN stack build --copy-bins --ghc-options -j4 --local-bin-path target
RUN mkdir -p /bins
RUN cp target/wallet-proxy-exe /bins/wallet-proxy

FROM ubuntu:19.10

EXPOSE 8950
EXPOSE 8888
EXPOSE 9090
EXPOSE 8900
EXPOSE 10000

RUN apt-get update && apt-get install -y unbound curl postgresql-server-dev-11 liblmdb0

COPY --from=build /build-project/baker_id_generator /baker_id_generator
COPY --from=build /build-project/start.sh /start.sh
COPY --from=build /build-project/genesis-data /genesis-data
COPY --from=build /build-project/genesis-complementary-bundle /genesis-complementary-bundle
COPY --from=build /build-project/p2p_client-cli /p2p_client-cli
COPY --from=build /build-project/p2p_bootstrapper-cli /p2p_bootstrapper-cli
COPY --from=build /build-project/node-collector /node-collector
COPY --from=build /build-project/node-collector-backend /node-collector-backend 
COPY --from=build /build-project/wallet_server /wallet-server
COPY --from=wallet-proxy-build /wallet-proxy/target/wallet-proxy-exe /wallet-proxy
COPY --from=wallet-proxy-build /libs/* /usr/lib/

ENTRYPOINT ["/start.sh"]
