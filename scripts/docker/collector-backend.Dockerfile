ARG base_image_tag
FROM concordium/base:${base_image_tag} as build
EXPOSE 8080
WORKDIR /build
COPY ./collector-backend .
ARG build_profile
RUN cargo build --"${build_profile}"

FROM ubuntu:20.04
ARG build_profile
COPY --from=build /build/target/"${build_profile}"/node-collector-backend /node-collector-backend
ENTRYPOINT ["/node-collector-backend"]
