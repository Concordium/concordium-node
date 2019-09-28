FROM archlinux/base

COPY deps/internal/consensus/stack.yaml /stack.yaml

RUN pacman -Sy && \
    pacman -Syyu --noconfirm && \
    pacman -S protobuf cmake clang git libtool rustup make m4 pkgconf autoconf automake \
        file which boost patch libunwind libdwarf elfutils unbound llvm capnproto numactl --noconfirm && \
    pacman -Scc --noconfirm && \
    rustup default 1.38.0 && \
    git clone https://github.com/libffi/libffi.git && \
    cd libffi && ./autogen.sh && ./configure && make -j$(nproc) && make install && \
    rm -rf libffi && \
    mkdir -p ~/.stack/global-project/ && \
    echo -e "packages: []\nresolver: $(cat /stack.yaml | grep ^resolver: | awk '{ print $NF }')" > ~/.stack/global-project/stack.yaml &&\
    curl -sSL https://get.haskellstack.org/ | sh
