# syntax=docker/dockerfile:experimental
FROM 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/base:0.2 as build

RUN pacman -Sy && \
    pacman -Syyu --noconfirm && \
    pacman -S protobuf cmake clang git libtool rustup make m4 pkgconf autoconf automake \
        file which boost patch libunwind libdwarf elfutils unbound llvm numactl --noconfirm && \
    pacman -Scc --noconfirm && \
    rustup default 1.38.0 && \
    git clone https://github.com/libffi/libffi.git && \
    cd libffi && ./autogen.sh && ./configure && make -j$(nproc) && make install && \
    rm -rf libffi

COPY . /build-project/
WORKDIR /build-project
COPY scripts/start.sh /build-project/start.sh
COPY scripts/init.build.env.sh /build-project/init.build.env.sh

COPY scripts/genesis-data ./genesis-data

# Build Environment: Hacl, ffi, Haskell (inherited from k8 build)
RUN ./init.build.env.sh

### Baker id gen
RUN \
    rustup install nightly-2019-07-10 && \
    cd baker_id_gen && \
    cargo +nightly-2019-07-10 build --release && \
    mv target/release/baker_id_gen ../baker_id_generator && \
    cd .. && \
    rm -rf baker_id_gen

### P2P client
RUN --mount=type=ssh cargo build --features=profiling,elastic_logging,collector

RUN chmod +x /build-project/start.sh

RUN cp /build-project/target/debug/p2p_client-cli /build-project/target/debug/p2p_bootstrapper-cli /build-project/target/debug/node-collector /build-project/

FROM ubuntu:19.10

EXPOSE 8950
EXPOSE 8888
EXPOSE 9090
EXPOSE 8900
EXPOSE 10000

RUN apt-get update && apt-get install -y unbound curl

COPY --from=build /build-project/baker_id_generator /baker_id_generator
COPY --from=build /build-project/start.sh /start.sh
COPY --from=build /build-project/genesis-data /genesis-data
COPY --from=build /build-project/p2p_client-cli /p2p_client-cli
COPY --from=build /build-project/p2p_bootstrapper-cli /p2p_bootstrapper-cli
COPY --from=build /build-project/node-collector /node-collector

ENTRYPOINT ["/start.sh"]
