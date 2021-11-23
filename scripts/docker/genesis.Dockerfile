# syntax=docker/dockerfile:experimental

# Usage:
#   DOCKER_BUILDKIT=1 docker build \
#      -t concordium/genesis:<tag> \
#      --build-arg=tag=<genesis-data-repo-tag> \
#      --build-arg=dir_path=<genesis-data-repo-path> \
#      -f genesis.Dockerfile \
#      --ssh=default \
#      .

FROM alpine/git:latest as data
RUN mkdir -p -m 0600 ~/.ssh && ssh-keyscan gitlab.com >> ~/.ssh/known_hosts
ARG tag
ARG dir_path
RUN --mount=type=ssh git clone --depth=1 --branch="${tag}" git@gitlab.com:Concordium/genesis-data.git /tmp/genesis-data && \
    mv "/tmp/genesis-data/${dir_path}" /genesis-data && rm -rf /tmp/genesis-data

FROM alpine:3
COPY --from=data /genesis-data/genesis.dat /genesis.dat
