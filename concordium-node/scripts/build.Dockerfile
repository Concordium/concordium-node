FROM concordium/base:latest
RUN cargo build
RUN chmod +x ./start.sh
