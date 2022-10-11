ARG ubuntu_version

# Build Debian packages.
FROM ubuntu:${ubuntu_version}
COPY scripts/db-exporter/build.sh /build.sh
COPY scripts/db-exporter/copy.sh /copy.sh
COPY scripts/db-exporter/database-exporter-publish.sh /database-exporter-publish.sh

COPY . /build

ARG version
WORKDIR /build
RUN VERSION="${version}" \
      /build.sh

ENTRYPOINT [ "/copy.sh" ]
