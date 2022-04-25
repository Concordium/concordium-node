ARG universal_image_name
FROM ${universal_image_name} AS build

FROM ubuntu:20.04
# P2P listen port.
EXPOSE 8888
# Prometheus port.
EXPOSE 9090
# GRPC port.
EXPOSE 10000
RUN apt-get update && \
    apt-get install -y libpq-dev && \
    rm -rf /var/lib/apt/lists/*
COPY --from=build /build/concordium-node/target/concordium-node /concordium-node
ENTRYPOINT ["/concordium-node"]
