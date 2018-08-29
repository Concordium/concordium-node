FROM concordium/base:latest
EXPOSE 8888
ENV MAX_NODES="10000"
ENV EXTERNAL_PORT="8888"
ENV EXTERNAL_IP="52.212.172.79"
ENV EXTRA_ARGS="--no-trust-bans"
ENV NODE_ID="0000000000000000000000000000000000000000000000000000000000000000"
ENV PROMETHEUS_PUSH_GW="prometheus-pushgateway.prometheus.svc.cluster.local:9091"
ENV PROMETHEUS_PUSH_JOBNAME="bootstrapper_push"
RUN cargo build --release
ENTRYPOINT ./target/release/p2p_bootstrapper-cli --id ${NODE_ID} --listen-port 8888 --max-nodes ${MAX_NODES} --external-ip ${EXTERNAL_IP} --external-port ${EXTERNAL_PORT} --prometheus-push-gateway ${PROMETHEUS_PUSH_GW} --prometheus-job-name ${PROMETHEUS_PUSH_JOBNAME} ${EXTRA_ARGS}


