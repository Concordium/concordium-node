ARG ubuntu_version
ARG ghc_version
# Build static libraries
FROM concordium/static-libraries:latest as static-builder
COPY . /build
WORKDIR /build
RUN GHC_VERSION="${ghc_version}" \
      /build/scripts/static-libraries/build-static-libraries.sh

# Build Debian packages.
FROM ubuntu:${ubuntu_version}
COPY --from=static-builder "/build/static-consensus-binaries-${ghc_version}.tar.gz" /tmp/static-consensus-binaries.tar.gz
RUN tar -C /tmp -xf /tmp/static-consensus-binaries.tar.gz && \
    mv /tmp/binaries/bin/database-exporter /database-exporter && \
    rm /tmp/static-consensus.tar.gz

COPY scripts/db-exporter/build.sh /build.sh
COPY scripts/db-exporter/copy.sh /copy.sh
COPY scripts/db-exporter/database-exporter-publish.sh /database-exporter-publish.sh
COPY . /build

ARG version
WORKDIR /build
RUN VERSION="${version}" \
      /build.sh

ENTRYPOINT [ "/copy.sh" ]
