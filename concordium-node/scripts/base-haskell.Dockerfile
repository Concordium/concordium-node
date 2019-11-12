FROM haskell:8.6.5
RUN apt-get update && apt-get install -y moreutils curl
RUN apt-get clean
RUN curl https://sh.rustup.rs -sSf | CARGO_HOME="/usr/local" RUSTUP_HOME="/usr/local" sh -s -- -y
RUN rustup default 1.39.0
RUN rustc --version && cargo --version