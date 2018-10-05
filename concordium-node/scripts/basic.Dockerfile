FROM concordium/build:latest
EXPOSE 10000
EXPOSE 8888
EXPOSE 9090
ENTRYPOINT ./start-node.sh 