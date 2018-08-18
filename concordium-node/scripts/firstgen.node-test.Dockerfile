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
COPY --from=build build-project/target/release/p2p_client-cli .
RUN apt-get update -yqq
RUN apt-get install libssl1.1
EXPOSE 10000
EXPOSE 8888
ENV EXTRA_ARGS="--debug"
ENV DESIRED_PEERS="50"
ENV PROMETHEUS_PUSH_GW="push-gw.test.concordium.com"
ENV PROMETHEUS_PUSH_GW_USER="testuser"
ENV PROMETHEUS_PUSH_GW_PASS="testpass"
ENV PROMETHEUS_PUSH_JOBNAME="firstgen_node_test_push"
CMD ["./p2p_client-cli", "--desired-nodes", "${DESIRED_PEERS}", "--prometheus-push-gateway", "${PROMETHEUS_PUSH_GW}", "--prometheus-push-gateway-username", "${PROMETHEUS_PUSH_GW_USER}", "--prometheus-push-gateway-password","${PROMETHEUS_PUSH_GW_PASS}","--prometheus-job-name","${PROMETHEUS_PUSH_JOBNAME}","${EXTRA_ARGS}"]