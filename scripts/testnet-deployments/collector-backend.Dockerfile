ARG base_image_tag
ARG build_type # either debug or release

FROM concordium/base:${base_image_tag} as build

ARG build_type

COPY ./collector-backend /build
WORKDIR /build

RUN cargo build --$build_type

FROM ubuntu:20.04

ARG build_type

# Port where the collector backend listens on by default.
EXPOSE 8080

COPY --from=build /build/target/$build_type/node-collector-backend /node-collector-backend

ENTRYPOINT ["/node-collector-backend"]
