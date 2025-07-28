ARG ubuntu_version
ARG static_binaries_image_tag
# Build static libraries
FROM static-node-binaries:$static_binaries_image_tag as binaries

# Build Debian packages.
FROM ubuntu:${ubuntu_version}
COPY --from=binaries /build/bin/p2p_bootstrapper-cli /tmp/p2p_bootstrapper-cli

COPY scripts/bootstrapper/build.sh /build.sh
COPY . /build

ARG version
WORKDIR /build
RUN VERSION="${version}" \
      /build.sh
RUN apt -y install /build/p2p-bootstrapper_${version}.deb

RUN mkdir -p /var/lib/concordium/data /var/lib/concordium/config
CMD p2p-bootstrapper --data-dir /var/lib/concordium/data --config-dir /var/lib/concordium/config
