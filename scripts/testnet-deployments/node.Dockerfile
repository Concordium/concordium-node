# syntax=docker/dockerfile:experimental

ARG universal_image_name

# Fetch genesis-data.
FROM alpine/git:latest as genesis-data
ARG genesis_ref
ARG genesis_path
RUN mkdir -p -m 0600 ~/.ssh && ssh-keyscan gitlab.com >> ~/.ssh/known_hosts
RUN --mount=type=ssh git clone --depth 1 --branch "${genesis_ref}" git@gitlab.com:Concordium/genesis-data.git
RUN mv "genesis-data/${genesis_path}" /data

FROM $universal_image_name AS build
FROM ubuntu:20.04

ARG build_type

# TODO only list actually used ports
EXPOSE 8950
EXPOSE 8888
EXPOSE 9090
EXPOSE 8900
EXPOSE 10000

# TODO only install actually used packages.
RUN apt-get update && \
    apt-get install -y ca-certificates libpq-dev && \
    rm -rf /var/lib/apt/lists/*

COPY --from=build /out/$build_type/concordium-node /concordium-node
COPY --from=build /out/start.sh /start.sh
COPY --from=genesis-data /data /genesis-data

ENTRYPOINT ["/start.sh"]
