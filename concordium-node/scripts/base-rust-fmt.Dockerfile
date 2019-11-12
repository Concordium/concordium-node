FROM rustlang/rust:nightly
RUN apt-get update && apt-get install -y moreutils curl
RUN apt-get clean
RUN rustup default nightly-2019-07-25
RUN rustup component add rustfmt
RUN rustc --version && cargo --version