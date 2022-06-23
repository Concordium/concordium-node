ARG universal_image_name
FROM ${universal_image_name} AS build

FROM ubuntu:20.04
RUN apt-get update && \
    apt-get install -y ca-certificates && \
    rm -rf /var/lib/apt/lists/*
COPY --from=build /build/concordium-node/target/release/node-collector /node-collector
ENTRYPOINT ["/node-collector"]
