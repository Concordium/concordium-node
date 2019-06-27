FROM archlinux/base
 
RUN pacman -Sy && \
    pacman -Syyu --noconfirm && \
    pacman -S protobuf cmake clang git libtool rustup make m4 pkgconf autoconf automake \
        file which boost patch libunwind libdwarf elfutils unbound llvm --noconfirm && \
    pacman -Scc --noconfirm && \
    rustup default stable && \
    git clone https://github.com/libffi/libffi.git && \
    cd libffi && ./autogen.sh && ./configure && make -j$(nproc) && make install && \
    rm -rf libffi && \
    mkdir -p ~/.stack/global-project/ && \
    echo -e "packages: []\nresolver: $(cat /stack.yaml | grep ^resolver: | awk '{ print $NF }')" > ~/.stack/global-project/stack.yaml &&\
    curl -sSL https://get.haskellstack.org/ | sh

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
    rustup install nightly-2019-03-22 && \
    cd baker_id_gen.git && \
    cargo +nightly-2019-03-22 build --release && \
    mv target/release/baker_id_gen .. && \
    cd .. && \
    rm -rf baker_id_gen.git

### P2P client
RUN \
    cd p2p-client && \
    cargo build

RUN chmod +x /build-project/start.sh

EXPOSE 8888 
