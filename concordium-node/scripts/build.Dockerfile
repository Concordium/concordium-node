FROM concordium/base:latest
RUN cargo build --release
RUN chmod +x ./start-bootstrapper.sh
