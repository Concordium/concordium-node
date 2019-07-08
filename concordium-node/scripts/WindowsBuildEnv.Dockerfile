FROM ubuntu
RUN apt-get update
RUN apt-get install -y apt-utils
RUN apt-get install -y yasm gcc-mingw-w64 cmake protobuf-compiler libprotobuf-dev g++-mingw-w64-x86-64 gcc-mingw-w64-x86-64 libnpth-mingw-w64-dev libz-mingw-w64 libz-mingw-w64-dev mingw-w64-common mingw-w64-tools mingw-w64-x86-64-dev mingw-w64 binutils-mingw-w64 curl libssl-dev dos2unix
RUN curl https://sh.rustup.rs -sSf | sh -s -- -y
RUN $HOME/.cargo/bin/rustup default 1.36.0
RUN $HOME/.cargo/bin/rustup target add x86_64-pc-windows-gnu
COPY scripts/mingw-config $HOME/.cargo/config
RUN curl -sSL https://get.haskellstack.org/ | sh

