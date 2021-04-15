ARG universal_image_name

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
    apt-get install -y unbound ca-certificates libpq-dev && \
    rm -rf /var/lib/apt/lists/*

COPY --from=build /out/$build_type/concordium-node /concordium-node
COPY --from=build /out/start.sh /start.sh
#COPY --from=build /out/genesis-data /genesis-data

ENTRYPOINT ["/start.sh"]
