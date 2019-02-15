FROM concordium/base:latest
RUN cargo build
RUN chmod +x ./start-bootstrapper.sh
RUN chmod +x ./start-ipdiscovery.sh
RUN chmod +x ./start-node.sh
RUN chmod +x ./start-testrunner.sh
RUN chmod +x ./start.sh