# syntax=docker/dockerfile:experimental
FROM 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/base:0.1 as build
COPY . /build-project
WORKDIR /build-project
COPY ./scripts/init.build.env.sh ./init.build.env.sh
COPY ./scripts/start.sh ./start.sh
COPY ./scripts/genesis-data ./genesis-data
ENV LD_LIBRARY_PATH=/usr/local/lib
RUN --mount=type=ssh ./init.build.env.sh 
# Build P2P client
RUN --mount=type=ssh cargo build --release --features=static,collector,beta && \
    strip /build-project/target/release/p2p_client-cli && \
    strip /build-project/target/release/node-collector && \
    cp /build-project/target/release/p2p_client-cli /build-project/ && \
    cp /build-project/target/release/node-collector /build-project/ && \
    cd /build-project/scripts/genesis-data && \
    tar -xf 20-bakers.tar.gz && \
    cd genesis_data && \
    cp genesis.dat /build-project/

FROM 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/base:0.1 as haskell-build
COPY ./src/proto/concordium_p2p_rpc.proto /concordium.proto
# P2P client is now built
RUN --mount=type=ssh pacman -Syy --noconfirm openssh && \
    mkdir -p -m 0600 ~/.ssh && ssh-keyscan gitlab.com >> ~/.ssh/known_hosts && \
    git clone git@gitlab.com:Concordium/consensus/simple-client.git && \
    cd simple-client && \
    git checkout http-server-interface && \
    git submodule update --init --recursive && \
    curl -s "https://s3-eu-west-1.amazonaws.com/static-libraries.concordium.com/static-consensus-binaries-$(git submodule | grep prototype | head -n1 | awk '{print $1}').tar.gz" -O && \
    tar -xf static-consensus-binaries-$(git submodule | grep prototype | head -n1 | awk '{print $1}').tar.gz && \
    mv binaries /genesis-binaries && \
    rm proto/concordium.proto && \
    cp /concordium.proto proto/concordium.proto && \
    ./build-deps.sh && \
    ./stack build && \
    mkdir -p /libs && \
    cp extra-libs/* /libs/ && \
    cp .stack-work/dist/*/*/build/middleware/middleware /middleware

FROM node:11 as node-build
WORKDIR /
RUN mkdir -p -m 0600 ~/.ssh && ssh-keyscan gitlab.com >> ~/.ssh/known_hosts
RUN --mount=type=ssh git clone git@gitlab.com:Concordium/node-dashboard.git
WORKDIR /node-dashboard
RUN git checkout 42c0d5ccf311843899bf4e9f3b5602c477314874
RUN npm i
RUN npm run build
ENV NODE_ENV=production
RUN npm run build

FROM ubuntu:19.10
EXPOSE 8888
EXPOSE 10000

ENV RPC_SERVER_ADDR=0.0.0.0
ENV MODE=basic
ENV BOOTSTRAP_FIRST_NODE=bootstrap.eu.test.concordium.com:8888
ENV DATA_DIR=/var/lib/concordium/data
ENV CONFIG_DIR=/var/lib/concordium/config
ENV EXTRA_ARGS="--no-dnssec"
ENV NODE_URL=localhost:10000
ENV COLLECTORD_URL=https://dashboard.eu.prod.concordium.com/post/nodes
ENV GRPC_HOST=localhost
ENV GRPC_PORT=10000
ENV DISTRIBUTION_CLIENT=true
ENV BAKER_ID=node-0
#ENV ES_URL=http://localhost:9200

RUN apt-get update && apt-get install -y unbound curl netbase ca-certificates supervisor nginx
COPY --from=build /build-project/p2p_client-cli /p2p_client-cli
COPY --from=build /build-project/node-collector /node-collector
COPY --from=build /build-project/start.sh /start.sh
COPY --from=build /build-project/genesis.dat /genesis.dat
COPY --from=haskell-build /libs/* /usr/lib/
COPY --from=haskell-build /middleware /middleware
COPY --from=haskell-build /genesis-binaries /genesis-binaries
COPY --from=node-build /node-dashboard/dist/public /var/www/html/

COPY ./scripts/supervisord.conf /etc/supervisor/supervisord.conf
COPY ./scripts/concordium.conf /etc/supervisor/conf.d/concordium.conf
COPY ./scripts/beta-client.sh /beta-client.sh

ENTRYPOINT [ "/beta-client.sh" ]
