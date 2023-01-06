# Static libraries scripts

Static libraries are built using an fPIC GHC created with
[these](https://gitlab.com/Concordium/devops/-/tree/master/fpic) scripts by
[this](http://jenkins.internal.concordium.com/job/fpic-ghc_jenkinsfile/) job.

The script is consumed by various Docker build jobs, usually as a designated "build static libraries" stage
before the "actual" build stage:

```dockerfile
# Build static libraries in base image 'concordium/static-libraries'.
FROM concordium/static-libraries:latest as static-builder
COPY . /build
ARG ghc_version
WORKDIR /build
RUN GHC_VERSION="${ghc_version}" \
      /build/scripts/static-libraries/build-static-libraries.sh

# Build binaries in base image 'concordium/base'.
FROM concordium/base:latest
COPY . /build
WORKDIR /build
# Copy static libraries that were built by the 'static-builder' into the correct place
# (/build/concordium-node/deps/static-libs/linux).
ARG ghc_version
COPY --from=static-builder "/build/static-consensus-${ghc_version}.tar.gz" /tmp/static-consensus.tar.gz
RUN tar -C /tmp -xf /tmp/static-consensus.tar.gz && \
    mkdir -p /build/concordium-node/deps/static-libs && \
    mv /tmp/target /build/concordium-node/deps/static-libs/linux && \
    rm /tmp/static-consensus.tar.gz
# Build binaries ...
```

It's considered a best practice to let this be followed by a final stage that copies the build artifacts into a fresh image.

# Replacement of integer-simple

Prior to [`concordium/static-libraries`](https://hub.docker.com/r/concordium/static-libraries/tags) version 0.30
the included GHC was using the integer library `integer-simple`.
Starting from version 0.30, the default GMP integer library is used.
The static build of `concordium-node` up until version 1.1.3 (inclusive) depends on `integer-simple`.
