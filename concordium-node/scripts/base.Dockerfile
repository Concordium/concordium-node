FROM base/archlinux
COPY . /build-project
WORKDIR /build-project
COPY ./scripts/init.build.env.sh ./init.build.env.sh
RUN pacman -Syy --noconfirm
RUN pacman -S archlinux-keyring --noconfirm
RUN pacman -Syu --noconfirm
RUN pacman -S protobuf cmake go clang rust git libtool rustup make m4 pkgconf openssl autoconf automake --noconfirm
RUN ./init.build.env.sh
RUN cargo build --release
