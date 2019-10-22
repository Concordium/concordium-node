# syntax=docker/dockerfile:experimental
FROM archlinux/base

ENV GHC_VERSION 8.6.5

COPY scripts/build-static-libraries.sh /build-static-libraries.sh
COPY scripts/static-libs /manifests
COPY deps/internal/consensus /build

RUN chmod +x /build-static-libraries.sh
WORKDIR /
RUN --mount=type=ssh ./build-static-libraries.sh
ENTRYPOINT ["./build-static-libraries-copy-out.sh"]