FROM concordium/genesis-tools:latest
RUN apt-get update && \
    apt-get -y install python3 && \
    rm -rf /var/lib/apt/lists/*

# Use globally installed tools.
ENV GENESIS_DAT_TOOL=genesis
ENV GENERATE_UPDATE_KEYS=generate-update-keys
ENV CLIENT_TOOL=client
ENV GENESIS_ACCOUNTS_TOOL=genesis_tool

WORKDIR /work
ENV GENESIS_DIR=/work/out

COPY ./generate-test-genesis.py /
ENTRYPOINT [ "/generate-test-genesis.py" ]
