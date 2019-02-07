FROM archlinux/base

COPY assets/ /build-project/
WORKDIR /build-project

ENV OPENSSL_LIB_DIR=/usr/lib/openssl-1.0 \
    OPENSSL_INCLUDE_DIR=/usr/include/openssl-1.0

RUN pacman -Sy &&\
    pacman -Syyu --noconfirm && \
    pacman -S protobuf openssl-1.0 cmake go clang git libtool rustup make \
        m4 pkgconf openssl autoconf automake ldns file which boost zstd patch \
        libunwind libdwarf elfutils unbound --noconfirm && \
    pacman -Scc --noconfirm && \
    chmod +x /build-project/*.sh && \
    ./init.build.env.sh 

RUN cd p2p-client && cargo build

EXPOSE 8888 
