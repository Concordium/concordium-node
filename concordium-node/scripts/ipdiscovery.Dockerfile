FROM concordium/build:latest
EXPOSE 8900
ENV PROMETHEUS_PUSH_GW="prometheus-pushgateway.prometheus.svc.cluster.local:9091"
ENV PROMETHEUS_PUSH_JOBNAME="ip_discovery_push"
ENV PROMETHEUS_INSTANCE_NAME="NODE_NAME_CHANGE"
ENTRYPOINT ./target/release/ip_discovery --listen-port 8900 --prometheus-push-gateway ${PROMETHEUS_PUSH_GW} --prometheus-job-name ${PROMETHEUS_PUSH_JOBNAME} ${EXTRA_ARGS}


