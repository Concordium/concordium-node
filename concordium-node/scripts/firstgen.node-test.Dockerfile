FROM concordium/base:latest
EXPOSE 10000
EXPOSE 8888
ENV EXTRA_ARGS="--debug"
ENV DESIRED_PEERS="50"
ENV PROMETHEUS_PUSH_GW="push-gw.test.concordium.com"
ENV PROMETHEUS_PUSH_GW_USER="testuser"
ENV PROMETHEUS_PUSH_GW_PASS="testpass"
ENV PROMETHEUS_PUSH_JOBNAME="firstgen_node_test_push"
ENV DNS_BOOSTRAP_NODE="bootstrap.concordium.com"
ENV TESTRUNNER_URL="http://testrunner.svc"
CMD ["./target/release/p2p_client-cli", "--desired-nodes", "${DESIRED_PEERS}", "--prometheus-push-gateway", "${PROMETHEUS_PUSH_GW}", "--prometheus-push-gateway-username", "${PROMETHEUS_PUSH_GW_USER}", "--prometheus-push-gateway-password","${PROMETHEUS_PUSH_GW_PASS}","--prometheus-job-name","${PROMETHEUS_PUSH_JOBNAME}","--bootstrap-server","${DNS_BOOSTRAP_NODE}","--testrunner-url","${TESTRUNNER_URL}", "${EXTRA_ARGS}"]