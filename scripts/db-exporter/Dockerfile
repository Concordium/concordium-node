ARG ubuntu_version
# ARG static_libraries_image_tag

# # Build static consensus libraries.
# FROM concordium/static-libraries:${static_libraries_image_tag} as static-builder
# COPY --from=source /concordium-node /build
# ARG ghc_version
# WORKDIR /build
# RUN GHC_VERSION="${ghc_version}" \
#       /build/scripts/static-libraries/build-static-libraries.sh

# Build Debian packages.
FROM ubuntu:${ubuntu_version}
COPY scripts/db-exporter/build.sh /build.sh
COPY scripts/db-exporter/copy.sh /copy.sh
COPY scripts/db-exporter/database-exporter-publish.sh /database-exporter-publish.sh

COPY . /build

# # Copy static libraries that were built by the static-builder into the correct place
# # (/build/concordium-node/deps/static-libs/linux).
# ARG ghc_version
# COPY --from=static-builder /build/static-consensus-${ghc_version}.tar.gz /tmp/static-consensus.tar.gz
# RUN tar -C /tmp -xf /tmp/static-consensus.tar.gz && \
#     mkdir -p /build/concordium-node/deps/static-libs && \
#     mv /tmp/target /build/concordium-node/deps/static-libs/linux && \
#     rm /tmp/static-consensus.tar.gz

# # Copy utils tools.
# COPY --from=static-builder /build/concordium-base/rust-bins/target/release/utils /build/concordium-utils

ARG version
WORKDIR /build
RUN VERSION="${version}" \
      /build.sh

ENTRYPOINT [ "/copy.sh" ]
