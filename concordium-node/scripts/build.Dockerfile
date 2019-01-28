FROM concordium/base:latest
RUN OPENSSL_LIB_DIR=/usr/lib/openssl-1.0 OPENSSL_INCLUDE_DIR=/usr/include/openssl-1.0 cargo build
RUN chmod +x ./start-bootstrapper.sh
RUN chmod +x ./start-ipdiscovery.sh
RUN chmod +x ./start-node.sh
RUN chmod +x ./start-testrunner.sh
RUN chmod +x ./start.sh