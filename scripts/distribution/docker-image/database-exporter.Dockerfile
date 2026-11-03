# syntax=docker/dockerfile:1

# Build the internal database exporter image.
# It takes database-exporter and the shared libraries from the artifacts stage of builder.Dockerfile.
# Pass the output of that stage as the artifacts build context.
# The image is amd64 only, like the reporter hosts.

# Keep this equal to UBUNTU_VERSION in builder.Dockerfile.
# The shared libraries are built against that release.
ARG UBUNTU_VERSION=26.04

# Install the AWS CLI.
# Change the version and the archive checksum together.
FROM ubuntu:${UBUNTU_VERSION} AS aws-cli

ARG DEBIAN_FRONTEND=noninteractive
ARG AWS_CLI_VERSION=2.37.5
ARG AWS_CLI_SHA256=850ba65f1342a1f725f4868de3c4621ea729af31dac96e54b574cbb0ef309029

SHELL ["/bin/bash", "-o", "pipefail", "-c"]

RUN apt-get update \
    && apt-get install --yes --no-install-recommends ca-certificates curl unzip \
    && rm -rf /var/lib/apt/lists/* \
    && curl --fail --location --show-error --silent \
        "https://awscli.amazonaws.com/awscli-exe-linux-x86_64-${AWS_CLI_VERSION}.zip" \
        --output /tmp/awscli.zip \
    && echo "${AWS_CLI_SHA256}  /tmp/awscli.zip" | sha256sum --check --strict \
    && unzip -q /tmp/awscli.zip -d /tmp \
    && /tmp/aws/install --install-dir /usr/local/aws-cli --bin-dir /usr/local/bin \
    && rm --recursive --force /tmp/aws /tmp/awscli.zip

FROM ubuntu:${UBUNTU_VERSION}

ARG DEBIAN_FRONTEND=noninteractive

# Install the native runtime libraries of the node image, plus jq and tini.
# Use the same unprivileged identity as the node image.
RUN apt-get update \
    && apt-get install --yes --no-install-recommends \
        ca-certificates \
        jq \
        libffi8 \
        libgmp10 \
        liblmdb0 \
        libnuma1 \
        libssl3 \
        libtinfo6 \
        tini \
        zlib1g \
    && rm -rf /var/lib/apt/lists/* \
    && groupadd --gid 10001 concordium \
    && useradd --uid 10001 --gid concordium --no-create-home --shell /usr/sbin/nologin concordium

COPY --from=aws-cli /usr/local/aws-cli/ /usr/local/aws-cli/
COPY --from=artifacts /runtime/usr/local/lib/ /usr/local/lib/
COPY --from=artifacts /exporter/database-exporter /usr/local/bin/database-exporter
COPY scripts/db-exporter/database-exporter-publish.sh /usr/local/bin/database-exporter-publish.sh

# Use the reporter host layout, with the data disk mounted at /var/lib/concordium.
# The export directory is on that disk.
# Register the copied libraries and fail the build if a tool cannot resolve its libraries or start.
RUN chmod 0755 /usr/local/bin/database-exporter-publish.sh \
    && ln --symbolic /usr/local/aws-cli/v2/current/bin/aws /usr/local/bin/aws \
    && ln --symbolic /var/lib/concordium/data/database_exporter /var/lib/database_exporter \
    && ldconfig \
    && ldd /usr/local/bin/database-exporter | tee /tmp/database-exporter.ldd \
    && ! grep --quiet 'not found' /tmp/database-exporter.ldd \
    && /usr/local/bin/database-exporter --help \
    && aws --version \
    && jq --version \
    && rm /tmp/database-exporter.ldd

USER concordium:concordium

# Run the publish script once, then exit.
ENTRYPOINT ["/usr/bin/tini", "--", "/usr/local/bin/database-exporter-publish.sh"]
