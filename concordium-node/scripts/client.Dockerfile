FROM 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/universal:VERSION_TAG AS wrapper
RUN ls /build-project

FROM ubuntu:19.10

EXPOSE 8950
EXPOSE 8888
EXPOSE 9090
EXPOSE 8900
EXPOSE 10000

RUN apt-get update && apt-get install -y unbound ca-certificates
COPY --from=wrapper /build-project/p2p_client-cli /p2p_client-cli
COPY --from=wrapper /build-project/start.sh /start.sh
COPY --from=wrapper /build-project/gen_data.sh /gen_data.sh
COPY --from=wrapper /build-project/genesis-data /genesis-data

ENTRYPOINT ["/start.sh"]