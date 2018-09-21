FROM concordium/build:latest
EXPOSE 8900
EXPOSE 9090
ENTRYPOINT ./start-ipdiscovery.sh