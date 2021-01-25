ARG universal_version

FROM 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/universal:$universal_version AS wrapper
FROM ubuntu:20.04

ARG build_type

EXPOSE 8950
EXPOSE 8888
EXPOSE 9090
EXPOSE 8900
EXPOSE 10000

RUN apt-get update && apt-get install -y unbound libpq-dev
COPY --from=wrapper /out/"$build_type"/p2p_bootstrapper-cli /p2p_bootstrapper-cli
COPY --from=wrapper /out/start.sh /start.sh

ENTRYPOINT ["/start.sh"]
