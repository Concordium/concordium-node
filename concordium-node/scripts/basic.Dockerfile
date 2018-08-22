FROM concordium/base:latest
EXPOSE 10000
EXPOSE 8888
ENV EXTRA_ARGS="--debug"
ENV DESIRED_PEERS="50"
ENV DNS_BOOSTRAP_NODE="ab01a2e83a55311e8bfae0abd5ece994-5702f657aebe8096.elb.eu-west-1.amazonaws.com"
RUN cargo build --release
CMD ["./target/release/p2p_client-cli", "--desired-nodes", "${DESIRED_PEERS}", "--bootstrap-server","${DNS_BOOSTRAP_NODE}","${EXTRA_ARGS}"]
