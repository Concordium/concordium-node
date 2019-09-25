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
RUN --mount=type=ssh cargo build --release --features=static,elastic_logging && \
    strip /build-project/target/release/p2p_client-cli && \
    cp /build-project/target/release/p2p_client-cli /build-project/ 
# P2P client is now built
RUN --mount=type=ssh pacman -Syy --noconfirm openssh && \
    mkdir -p -m 0600 ~/.ssh && ssh-keyscan gitlab.com >> ~/.ssh/known_hosts && \
    git clone git@gitlab.com:Concordium/consensus/simple-client.git && \
    cd simple-client && \
    git checkout http-server-interface && \
    git submodule update --init --recursive && \
    rm proto/concordium.proto && \
    cp /build-project/src/proto/concordium_p2p_rpc.proto proto/concordium.proto && \
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
ENV ES_URL=http://localhost:9200

RUN apt-get update && apt-get install -y unbound curl netbase ca-certificates supervisor nginx
COPY --from=build /build-project/p2p_client-cli /p2p_client-cli
COPY --from=build /build-project/start.sh /start.sh
COPY --from=build /libs/* /usr/lib/
COPY --from=build /middleware /middleware
COPY --from=node-build /node-dashboard/dist/public/* /var/www/html/

COPY ./scripts/supervisord.conf /etc/supervisor/supervisord.conf
COPY ./scripts/concordium.conf /etc/supervisor/conf.d/concordium.conf

RUN groupadd -g 61000 docker
RUN useradd -g 61000 -l -M -s /bin/false -u 61000 docker

RUN mkdir -p ${DATA_DIR} && mkdir -p ${CONFIG_DIR} && chown -R docker:docker ${DATA_DIR} && chown -R docker:docker ${CONFIG_DIR} && \
    curl https://artifacts.elastic.co/downloads/elasticsearch/elasticsearch-oss-7.1.1-amd64.deb -O && \
    dpkg -i elasticsearch-oss-7.1.1-amd64.deb && \
    mkdir -p /var/log/elasticsearch && \
    mkdir -p /var/lib/elasticsearch && \
    chown -R elasticsearch:elasticsearch /var/log/elasticsearch && \
    chown -R elasticsearch:elasticsearch /var/lib/elasticsearch

ENTRYPOINT [ "/usr/bin/supervisord", "-c", "/etc/supervisor/conf.d/concordium.conf" ]
