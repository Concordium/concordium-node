FROM concordium/build:latest
EXPOSE 8950
EXPOSE 8888
ENV EXTRA_ARGS="--debug"
ENV DNS_BOOSTRAP_NODE="bootstrap.test.p2p.concordium.com"
ENV LISTEN_PORT=8888
ENV LISTEN_HTTP_PORT=8950
ENTRYPOINT ./start-ipdiscovery.sh