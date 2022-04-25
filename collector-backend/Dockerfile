ARG base_image_tag
FROM concordium/base:${base_image_tag} as build
EXPOSE 8080
WORKDIR /build
COPY . .
RUN cargo build --release

FROM ubuntu:20.04
COPY --from=build /build/target/release/node-collector-backend /node-collector-backend
ENTRYPOINT ["/node-collector-backend"]
