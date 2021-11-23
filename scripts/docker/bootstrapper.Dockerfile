ARG universal_image_name
FROM ${universal_image_name} AS build

FROM ubuntu:20.04
EXPOSE 8888
RUN apt-get update && \
    apt-get install -y libpq-dev && \
    rm -rf /var/lib/apt/lists/*
ARG build_profile
COPY --from=build "/target/${build_profile}/p2p_bootstrapper-cli" /p2p_bootstrapper-cli
ENTRYPOINT ["/p2p_bootstrapper-cli"]
