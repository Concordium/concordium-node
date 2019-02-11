FROM archlinux/base

RUN pacman -Sy &&\
    pacman -Syyu --noconfirm && \
    pacman -S protobuf openssl-1.0 cmake go clang git libtool rustup make \
        m4 pkgconf openssl autoconf automake ldns file which boost zstd patch \
        libunwind libdwarf elfutils unbound openssh --noconfirm && \
    pacman -Scc --noconfirm

ADD assets/build-project/ /build-project/

ENV OPENSSL_LIB_DIR=/usr/lib/openssl-1.0 \
    OPENSSL_INCLUDE_DIR=/usr/include/openssl-1.0 \
    LD_LIBRARY_PATH=/usr/local/lib:$HOME/.stack/programs/x86_64-linux/ghc-tinfo6-8.4.3/lib/ghc-8.4.3/rts

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
    # Rust
    ldconfig && \
    rustup default nightly-2019-01-22

WORKDIR /build-project
ADD assets/sshkeys/* /root/.ssh/

### Baket id gen
RUN \
    git clone https://gitlab.com/fmgr/baker_id_gen.git baker_id_gen.git && \
    cd baker_id_gen.git && \
    cargo build --release && \
    mv target/release/baker_id_gen .. && \
    cd .. && \
    rm -rf baker_id_gen.git


### Consensus and stacks
RUN \
    git clone -b oak-integration --single-branch git@gitlab.com:Concordium/consensus/prototype.git consensus && \
    cd consensus && \
    git submodule update --init --recursive && \
    stack build --ghc-options '-dynamic' --force-dirty && \
    # Install and rename generated libs
    cp .stack-work/install/x86_64-linux-tinfo6/lts-12.19/8.4.4/lib/x86_64-linux-ghc-8.4.4/libHS*.so  /usr/local/lib && \
    find /usr/local/lib -iname libHSConcordium\*.so \
        -exec ln -s {} /usr/local/lib/libHSConcordium-0.1.0.0.so \; && \
    find /usr/local/lib -iname libHSlanguage-glsl-0.3.0-\*.so \
        -exec ln -s {} /usr/local/lib/libHSlanguage-glsl-0.3.0.so \; && \
    find /usr/local/lib -iname libHSoak-0.19.0-\*.so \
        -exec ln -s {} /usr/local/lib/libHSoak-0.19.0.so \; && \
    find /usr/local/lib -iname libHSmonadplus-1.3-\*.so \
        -exec ln -s {} /usr/local/lib/libHSmonadplus-1.3.so \; && \
    find /usr/local/lib -iname libHSconcordium-crypto-0.1-\*.so \
        -exec ln -s {} /usr/local/lib/libHSconcordium-crypto-0.1.so \; && \
    find ~/.stack/programs -name libHS\*-\*.so \
        -exec cp {} /usr/local/lib \; && \
    ls /usr/local/lib && \
    ls ~/.stack/programs/x86_64-linux/ && \
    cd ..  && \ 
    cp -R consensus/workdir .  && \
    mkdir -p ~/.stack/global-project/ && \
    cp /build-project/stack/stack.yaml ~/.stack/global-project/stack.yaml 

### P2P client
RUN \
    git clone git@gitlab.com:Concordium/p2p-client.git p2p-client && \
    cd p2p-client && \
    git checkout feature/docker-compose-local && \
    cargo build

# Copy entrypoints
COPY assets/entrypoints/ /build-project/

EXPOSE 8888 
