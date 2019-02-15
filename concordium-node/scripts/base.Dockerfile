FROM archlinux/base
COPY . /build-project
WORKDIR /build-project
COPY ./scripts/init.build.env.sh ./init.build.env.sh
COPY ./scripts/start-bootstrapper.sh ./start-bootstrapper.sh
COPY ./scripts/start-ipdiscovery.sh ./start-ipdiscovery.sh
COPY ./scripts/start-node.sh ./start-node.sh
COPY ./scripts/start-testrunner.sh ./start-testrunner.sh
COPY ./scripts/gen_data.sh ./gen_data.sh
COPY ./scripts/start.sh ./start.sh
RUN pacman -Sy &&\
    pacman -Syyu --noconfirm && \
    pacman -S protobuf cmake go clang git libtool rustup make m4 pkgconf openssl autoconf automake ldns file which boost zstd patch libunwind libdwarf elfutils unbound --noconfirm && \
    pacman -Scc --noconfirm && \
    ./init.build.env.sh