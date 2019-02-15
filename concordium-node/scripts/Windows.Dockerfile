FROM concordium/windowsbuildenv
COPY . /workdir
COPY scripts/init.win.build.env.sh /workdir/init.win.build.env.sh
WORKDIR /workdir
RUN dos2unix -o ./init.win.build.env.sh
RUN ./init.win.build.env.sh
RUN UNBOUND_DIR=/workdir/deps/windows/libunbound OPENSSL_STATIC=0 OPENSSL_INCLUDE_DIR=/workdir/deps/windows/openssl-1.1.1a-win64-mingw/include OPENSSL_LIB_DIR=/workdir/deps/windows/openssl-1.1.1a-win64-mingw/lib RUST_LOG=error $HOME/.cargo/bin/cargo build -v --target=x86_64-pc-windows-gnu
