FROM archlinux/base
 
RUN pacman -Sy &&\
    pacman -Syyu --noconfirm && \
    pacman -S protobuf cmake go clang git libtool rustup make \
        m4 pkgconf openssl autoconf automake ldns file which boost zstd patch \
        libunwind libdwarf elfutils unbound openssh dos2unix --noconfirm && \
    pacman -Scc --noconfirm

ADD assets/build-project/ /build-project/
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

### Baket id gen
RUN \
    cd baker_id_gen.git && \
    cargo build --release && \
    mv target/release/baker_id_gen .. && \
    cd .. && \
    rm -rf baker_id_gen.git

### Consensus and stacks
RUN \
    cd consensus && \
    stack build --ghc-options '-dynamic' --force-dirty && \
    # Install and rename generated libs
    cp .stack-work/install/x86_64-linux-tinfo6/lts-12.19/8.4.4/lib/x86_64-linux-ghc-8.4.4/libHS*.so  /usr/local/lib && \
    find /usr/local/lib -iname libHSConcordium-0.1.0.0-\*.so \
        -exec ln -s {} /usr/local/lib/libHSConcordium-0.1.0.0.so \; && \
    find /usr/local/lib -iname libHSlanguage-glsl-0.3.0-\*.so \
        -exec ln -s {} /usr/local/lib/libHSlanguage-glsl-0.3.0.so \; && \
    find /usr/local/lib -iname libHSoak-0.19.0-\*.so \
        -exec ln -s {} /usr/local/lib/libHSoak-0.19.0.so \; && \
    find /usr/local/lib -iname libHSmonadplus-1.3-\*.so \
        -exec ln -s {} /usr/local/lib/libHSmonadplus-1.3.so \; && \
    find /usr/local/lib -iname libHSconcordium-crypto-0.1-\*.so \
        -exec ln -s {} /usr/local/lib/libHSconcordium-crypto-0.1.so \; && \
    ldconfig && \
    cd ..  && \ 
    cp -R consensus/workdir .  && \
    rm -rf consensus && \
    mkdir -p ~/.stack/global-project/ && \
    cp /build-project/stack/stack.yaml ~/.stack/global-project/stack.yaml 

### P2P client
RUN \
    cd p2p-client && \
    cargo build

# Copy entrypoints
COPY assets/entrypoints/ /build-project/

EXPOSE 8888 
