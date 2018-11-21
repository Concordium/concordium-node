FROM base/archlinux
COPY . /build-project
WORKDIR /build-project
COPY ./scripts/init.build.env.sh ./init.build.env.sh
COPY ./scripts/start-bootstrapper.sh ./start-bootstrapper.sh
COPY ./scripts/start-ipdiscovery.sh ./start-ipdiscovery.sh
COPY ./scripts/start-node.sh ./start-node.sh
COPY ./scripts/start-testrunner.sh ./start-testrunner.sh
RUN pacman -Sy
RUN pacman -S reflector --noconfirm
RUN reflector --verbose --latest 5 --sort rate --save /etc/pacman.d/mirrorlist
RUN pacman -Syy --noconfirm
RUN pacman -S archlinux-keyring --noconfirm
RUN pacman -Syu --noconfirm
RUN pacman -S protobuf cmake go clang rust git libtool rustup make m4 pkgconf openssl autoconf automake ldns boost zstd patch libunwind libdwarf elfutils unbound --noconfirm
RUN pacman -Scc --noconfirm
# Include older OpenSSL for linking until dependencies are updated to allow for 1.1.1
RUN pacman -S openssl-1.0 --noconfirm
RUN ./init.build.env.sh
