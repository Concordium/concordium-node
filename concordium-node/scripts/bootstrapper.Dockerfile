FROM concordium/build:latest
EXPOSE 8888
ENV MAX_NODES="10000"
ENV EXTERNAL_PORT="8888"
ENV EXTRA_ARGS="--no-trust-bans"
ENV PROMETHEUS_PUSH_GW="prometheus-pushgateway.prometheus.svc.cluster.local:9091"
ENV PROMETHEUS_PUSH_JOBNAME="bootstrapper_push"
ENTRYPOINT ./start-bootstrapper.sh --listen-port 8888 --max-nodes ${MAX_NODES} --external-port ${EXTERNAL_PORT} --prometheus-push-gateway ${PROMETHEUS_PUSH_GW} --prometheus-job-name ${PROMETHEUS_PUSH_JOBNAME} ${EXTRA_ARGS}


