FROM archlinux/base
 
RUN pacman -Sy &&\
    pacman -Syyu --noconfirm && \
    pacman -S protobuf cmake go clang git libtool rustup make \
        m4 pkgconf autoconf automake ldns file which boost patch \
        libunwind libdwarf elfutils unbound openssh dos2unix --noconfirm && \
    pacman -Scc --noconfirm

ADD assets/build-project/ /build-project/
COPY assets/repos/p2p-client/scripts/start.sh /build-project/
WORKDIR /build-project

ENV LD_LIBRARY_PATH=/usr/local/lib:/root/.stack/programs/x86_64-linux/ghc-tinfo6-8.4.4/lib/ghc-8.4.4/rts

# Build Environment: Hacl, ffi, Haskell,
RUN \
    ### Build Hacl
    git clone https://github.com/mitls/hacl-c && \
    cd hacl-c && \
    make && \
    cp libhacl.so /usr/lib && \ 
    cd .. && \
    rm -rf hacl-c && \
    ### Build ffi
    git clone https://github.com/libffi/libffi.git && \
    cd libffi && \
    ./autogen.sh && \
    ./configure && \
    make -j8 && \
    make install && \
    cd .. && \
    rm -rf libffi && \
    ### Install Haskell 
    curl -sSL https://get.haskellstack.org/ | sh && \
    ### Heaptrack 
    git clone https://github.com/KDE/heaptrack.git && \
    cd heaptrack && \
    patch src/track/heaptrack.sh.cmake \
        /build-project/heaptrack-conf/include-date-in-name.patch && \
    mkdir build && \
    cd build && \
    cmake -DCMAKE_BUILD_TYPE=release .. && \
    make -j$(nproc) && \
    make install && \
    cd .. && \
    rm -rf heaptrack && \
    rm -rf include-date-in-name && \
    ldconfig && \
    rustup default nightly-2019-01-22

ADD assets/repos/ /build-project/

### Baker id gen
RUN \
    cd baker_id_gen.git && \
    cargo build --release && \
    mv target/release/baker_id_gen .. && \
    cd .. && \
    rm -rf baker_id_gen.git

### Consensus and stacks
RUN \
    (mkdir -p ~/.stack/global-project/ && echo -e "packages: []\nresolver: $(cat p2p-client/deps/internal/consensus/stack.yaml | grep ^resolver: | awk '{ print $NF }')" > ~/.stack/global-project/stack.yaml) && \
    cd p2p-client/deps/internal/consensus && \
    stack build --ghc-options '-dynamic' --force-dirty && \
    # Install and rename generated libs
    cp .stack-work/install/x86_64-linux-tinfo6/$(cat stack.yaml | grep ^resolver: | awk '{ print $NF }')/8.4.4/lib/x86_64-linux-ghc-8.4.4/libHS*.so  /usr/local/lib && \
    find /usr/local/lib -name libHSConcordium\*.so -exec ln -s {} /usr/local/lib/libHSConcordium-0.1.0.0.so \; && \
    find /usr/local/lib -name libHSacorn\*.so -exec ln -s {} /usr/local/lib/libHSacorn-0.1.0.0.so \; && \
    find /usr/local/lib -name libHSconcordium-crypto\*.so -exec ln -s {} /usr/local/lib/libHSconcordium-crypto-0.1.so \; &&\
    find ~/.stack/programs -name libHS\*-\*.so -exec cp {} /usr/local/lib \; && \
    ldconfig && \
    cd ../../../


### P2P client
RUN \
    # Build Concordium Crypto
    mkdir -p p2p-client/deps/internal/crypto/build && \
    cd p2p-client/deps/internal/crypto/build && \
    cmake -DCMAKE_BUILD_TYPE=Release .. && \
    cmake --build . && \
    cmake --build . --target install && \
    cd ../../../../../ && \
    # Build P2P client
    cd p2p-client && \
    cargo build

RUN chmod +x /build-project/start.sh

EXPOSE 8888 
