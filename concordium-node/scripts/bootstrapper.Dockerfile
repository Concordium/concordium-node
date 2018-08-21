FROM concordium/base:latest
EXPOSE 8888
ENV MAX_NODES="10000"
ENV EXTERNAL_PORT="8888"
ENV EXTERNAL_IP="0.0.0.0"
ENV EXTRA_ARGS="--debug --no-trust-bans"
ENTRYPOINT ./target/release/p2p_bootstrapper-cli --listen-port 8888 --max-nodes ${MAX_NODES} --external-ip ${EXTERNAL_IP} --external-port ${EXTERNAL_PORT} ${EXTRA_ARGS}


