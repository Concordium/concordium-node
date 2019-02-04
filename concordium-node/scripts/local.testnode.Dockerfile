FROM concordium/build:latest
WORKDIR /build-project
COPY ./scripts/local-start-node.sh ./start-node.sh
RUN chmod +x ./start-node.sh
EXPOSE 10000
EXPOSE 8888
ENV EXTRA_ARGS="--debug"
ENV DESIRED_PEERS="10"
ENV EXTERNAL_PORT="8889"
ENV BOOTSTRAP_NODE="127.0.0.1:8888"
ENTRYPOINT ./start-node.sh
