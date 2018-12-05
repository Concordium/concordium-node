FROM base/archlinux
COPY . /build-project
WORKDIR /build-project
COPY ./scripts/init.build.env.sh ./init.build.env.sh
COPY ./scripts/start-bootstrapper.sh ./start-bootstrapper.sh
COPY ./scripts/start-ipdiscovery.sh ./start-ipdiscovery.sh
COPY ./scripts/start-node.sh ./start-node.sh
COPY ./scripts/start-testrunner.sh ./start-testrunner.sh
RUN pacman -Sy &&\ 
    pacman -S reflector --noconfirm &&\
    pacman -Syy --noconfirm && \ 
    pacman -S archlinux-keyring --noconfirm &&\ 
    pacman -Syu --noconfirm &&\ 
    pacman -S protobuf openssl-1.0 cmake go clang rust git\
    libtool rustup make m4 pkgconf openssl autoconf\ 
    automake ldns boost zstd patch libunwind libdwarf \
    elfutils unbound --noconfirm &&\ 
    pacman -Scc --noconfirm && \
    ./init.build.env.sh