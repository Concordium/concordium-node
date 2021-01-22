FROM 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/universal:VERSION_TAG AS wrapper
RUN ls /build-project

FROM ubuntu:20.04

EXPOSE 8950
EXPOSE 8888
EXPOSE 9090
EXPOSE 8900
EXPOSE 10000

RUN apt-get update && apt-get install -y unbound ca-certificates libpq-dev
COPY --from=wrapper /build-project/BUILD_TYPE/node-collector /node-collector
COPY --from=wrapper /build-project/start.sh /start.sh

ENTRYPOINT ["/start.sh"]