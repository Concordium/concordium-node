FROM rust:latest as build
RUN USER=root cargo new --bin build-project
WORKDIR /build-project
COPY ./Cargo.lock ./Cargo.lock
COPY ./Cargo.toml ./Cargo.toml
RUN echo "deb http://ftp.debian.org/debian stretch-backports main" >> /etc/apt/sources.list
RUN apt-get update -yqq
RUN apt-get -t stretch-backports install -yqq --no-install-recommends build-essential
RUN apt-get -t stretch-backports install -y cmake golang-go
RUN git clone https://github.com/mitls/hacl-c
RUN ( cd hacl-c && make && cp libhacl.so /usr/lib );
RUN rm -rf hacl-c
RUN wget https://github.com/google/protobuf/archive/v3.5.1.tar.gz
RUN tar xzf v3.5.1.tar.gz
RUN ( cd protobuf-3.5.1 && autoreconf -i &&  ./configure --prefix=/usr && make && make install )
RUN rm -rf protobuf-3.5.1
RUN ldconfig
RUN rustup default nightly
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
CMD ["./p2p_client-cli"]
