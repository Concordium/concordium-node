ARG ubuntu_version
ARG static_binaries_image_tag
# Build static libraries
FROM static-node-binaries:$static_binaries_image_tag as binaries

# Build Debian packages.
FROM ubuntu:${ubuntu_version}
COPY --from=binaries /build/bin/p2p_bootstrapper-cli /tmp/p2p_bootstrapper-cli

COPY scripts/bootstrapper/build.sh /build.sh
COPY scripts/bootstrapper/copy.sh /copy.sh
COPY . /build

ARG version
WORKDIR /build
RUN VERSION="${version}" \
      /build.sh

ENTRYPOINT [ "/copy.sh" ]
