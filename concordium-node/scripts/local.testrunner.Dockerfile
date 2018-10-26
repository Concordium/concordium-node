FROM concordium/build:latest
WORKDIR /build-project
COPY ./scripts/local-start-testrunner.sh ./start-testrunner.sh
RUN chmod +x ./start-testrunner.sh
EXPOSE 10000
EXPOSE 8888
EXPOSE 8950
ENV EXTRA_ARGS="--debug"
ENV DESIRED_PEERS="20"
ENV EXTERNAL_PORT="8889"
ENV BOOTSTRAP_NODE="127.0.0.1:8888"
ENTRYPOINT ./start-testrunner.sh
