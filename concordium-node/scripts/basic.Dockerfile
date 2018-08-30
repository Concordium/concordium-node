FROM concordium/build:latest
EXPOSE 10000
EXPOSE 8888
ENV EXTRA_ARGS="--debug"
ENV DESIRED_PEERS="50"
ENV DNS_BOOSTRAP_NODE="bootstrap.eu.prod.concordium.com"
ENTRYPOINT ./target/release/p2p_client-cli --desired-nodes ${DESIRED_PEERS} --bootstrap-server ${DNS_BOOSTRAP_NODE} ${EXTRA_ARGS}
