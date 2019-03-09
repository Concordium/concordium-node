FROM archlinux/base
 
RUN pacman -Sy &&\
    pacman -Syyu --noconfirm && \
    pacman -S protobuf cmake go clang git libtool rustup make \
        m4 pkgconf autoconf automake ldns file which boost patch \
        libunwind libdwarf elfutils unbound openssh dos2unix --noconfirm && \
    pacman -Scc --noconfirm

COPY assets/repos/p2p-client/scripts/start.sh /build-project/
WORKDIR /build-project
ADD assets/repos/ /build-project/
ADD assets/repos/p2p-client/scripts/init.build.env.sh /build-project/p2p-client/init.build.env.sh

# Build Environment: Hacl, ffi, Haskell (inherited from k8 build)
RUN \ 
     (cd p2p-client &&\
    ./init.build.env.sh )

### Baker id gen
RUN \
    cd baker_id_gen.git && \
    cargo build --release && \
    mv target/release/baker_id_gen .. && \
    cd .. && \
    rm -rf baker_id_gen.git

### P2P client
RUN \
    cd p2p-client && \
    cargo build

RUN chmod +x /build-project/start.sh

EXPOSE 8888 
