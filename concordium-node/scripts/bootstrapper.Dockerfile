FROM rust:latest as build
RUN USER=root cargo new --bin build-project
WORKDIR /build-project
COPY ./Cargo.lock ./Cargo.lock
COPY ./Cargo.toml ./Cargo.toml
COPY ./scripts/init.build.env.sh ./init.build.env.sh
RUN ./init.build.env.sh
RUN echo "fn main() {}" > build.rs
RUN echo "pub fn _unused() {} " > src/lib.rs
RUN cargo build --release
RUN rm src/*.rs
COPY ./src ./src
COPY ./build.rs ./build.rs
RUN cargo build --release
FROM debian:stretch-slim
COPY --from=build build-project/target/release/p2p_bootstrapper-cli .
RUN apt-get update -yqq
RUN apt-get install libssl1.1
EXPOSE 8888
ENV MAX_NODES="10000"
ENV EXTERNAL_PORT="8888"
ENV EXTERNAL_IP="0.0.0.0"
ENV EXTRA_ARGS="--debug --no-trust-bans"
CMD ["./p2p_bootstrapper-cli", "--listen-port", "8888", "--max-nodes", "${MAX_NODES}", "--external-ip", "${EXTERNAL_IP}",  "--external-port", "${EXTERNAL_PORT}", "${EXTRA_ARGS}"]
