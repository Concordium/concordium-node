FROM concordium/build:latest
EXPOSE 8950
EXPOSE 8888
ENTRYPOINT ./start-testrunner.sh