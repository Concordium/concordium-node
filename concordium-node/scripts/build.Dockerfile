FROM concordium/base:latest
RUN OPENSSL_LIB_DIR=/usr/lib/openssl-1.0 OPENSSL_INCLUDE_DIR=/usr/include/openssl-1.0 cargo build --release
RUN chmod +x ./start-bootstrapper.sh
