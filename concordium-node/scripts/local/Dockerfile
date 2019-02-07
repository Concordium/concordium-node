FROM archlinux/base

RUN pacman -Sy &&\
    pacman -Syyu --noconfirm && \
    pacman -S protobuf openssl-1.0 cmake go clang git libtool rustup make \
        m4 pkgconf openssl autoconf automake ldns file which boost zstd patch \
        libunwind libdwarf elfutils unbound --noconfirm && \
    pacman -Scc --noconfirm

RUN mkdir /build-project/ && \
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
    curl -sSL https://get.haskellstack.org/ | sh 

ENV OPENSSL_LIB_DIR=/usr/lib/openssl-1.0 \
    OPENSSL_INCLUDE_DIR=/usr/include/openssl-1.0 \
    LD_LIBRARY_PATH=/usr/local/lib:$HOME/.stack/programs/x86_64-linux/ghc-tinfo6-8.4.3/lib/ghc-8.4.3/rts

COPY assets/ /build-project/
WORKDIR /build-project

### Install Consensus
RUN \
    chmod +x /build-project/*.sh && \
    cd consensus && \
    stack build --ghc-options '-dynamic' --force-dirty && \
    cp .stack-work/install/x86_64-linux-tinfo6/lts-12.19/8.4.4/lib/x86_64-linux-ghc-8.4.4/libHS*.so  /usr/local/lib && \
    find /usr/local/lib -name libHSConcordium\*.so \
        -exec ln -s {} /usr/local/lib/libHSConcordium-0.1.0.0.so \; && \
    find /usr/local/lib -name libHSlanguage-glsl-0.3.0-\*.so \
        -exec ln -s {} /usr/local/lib/libHSlanguage-glsl-0.3.0.so \; && \
    find /usr/local/lib -name libHSoak-0.19.0-\*.so \
        -exec ln -s {} /usr/local/lib/libHSoak-0.19.0.so \; && \
    find ~/.stack/programs -name libHS\*-\*.so \
        -exec cp {} /usr/local/lib \; && \
    ls /usr/local/lib && \
    ls ~/.stack/programs/x86_64-linux/ && \
    cd ..  && \ 
    cp -R consensus/workdir . 

### Stack
RUN mkdir -p ~/.stack/global-project/ && \
    cp /build-project/stack/stack.yaml ~/.stack/global-project/stack.yaml && \
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
    ### Rust 
    ldconfig && \
    rustup default nightly

RUN cd p2p-client && \
    cargo build

EXPOSE 8888 
