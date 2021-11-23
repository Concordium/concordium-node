ARG universal_image_name
FROM ${universal_image_name} AS build

FROM ubuntu:20.04
RUN apt-get update && \
    apt-get install -y libpq-dev && \
    rm -rf /var/lib/apt/lists/*
ARG build_profile
COPY --from=build "/target/${build_profile}/node-collector" /node-collector
ENTRYPOINT ["/node-collector"]
