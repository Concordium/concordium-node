# syntax=docker/dockerfile:experimental
FROM 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/base:0.15 as build
ARG consensus_type
ENV CONSENSUS_TYPE=$consensus_type
ARG consensus_profiling=false
ENV CONSENSUS_PROFILING=$consensus_profiling
COPY . /build-project
WORKDIR /build-project
COPY ./scripts/init.build.env.sh ./init.build.env.sh
COPY ./scripts/start.sh ./start.sh
COPY ./genesis-data ./genesis-data
COPY ./scripts/build-binaries.sh ./build-binaries.sh
ENV LD_LIBRARY_PATH=/usr/local/lib
RUN --mount=type=ssh ./init.build.env.sh
# Build P2P client
RUN --mount=type=ssh ./build-binaries.sh "collector,staging_net" release && \
    strip /build-project/target/release/p2p_client-cli && \
    strip /build-project/target/release/node-collector && \
    cp /build-project/target/release/p2p_client-cli /build-project/ && \
    cp /build-project/target/release/node-collector /build-project/ && \
    cd /build-project/genesis-data && \
    tar -xf 20-bakers.tar.gz && \
    cd genesis_data && \
    sha256sum genesis.dat && \
    cp genesis.dat /build-project/
# P2P client is now built
FROM 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/base:0.15 as haskell-build
COPY ./CONSENSUS_VERSION /CONSENSUS_VERSION
# Build middleware and concordium-client
RUN --mount=type=ssh mkdir -p -m 0600 ~/.ssh && ssh-keyscan gitlab.com >> ~/.ssh/known_hosts && \
    git clone --branch v0.3.1 --depth 1 --recurse-submodules git@gitlab.com:Concordium/consensus/simple-client.git && \
    cd simple-client && \
    mkdir -p ~/.stack/global-project/ && \
    echo -e "packages: []\nresolver: $(cat stack.yaml | grep ^resolver: | awk '{ print $NF }')" > ~/.stack/global-project/stack.yaml && \
    curl -sSL https://get.haskellstack.org/ | sh && \
    curl -s "https://s3-eu-west-1.amazonaws.com/static-libraries.concordium.com/static-consensus-binaries-$(cat /CONSENSUS_VERSION).tar.gz" -O && \
    tar -xf static-consensus-binaries-$(cat /CONSENSUS_VERSION).tar.gz && \
    mv binaries /genesis-binaries && \
    ./build-deps.sh && \
    for f in $(find . -type f -name package.yaml); do sed -i -e 's/[\s]*ld-options://g' -e 's/[\s]*- -static//g' $f; done && \
    ./stack build --flag "simple-client:middleware" && \
    mkdir -p /libs && \
    cp extra-libs/* /libs/ && \
    cp .stack-work/dist/*/*/build/middleware/middleware /middleware && \
    cp .stack-work/dist/*/*/build/concordium-client/concordium-client /concordium-client-bin && \
    strip /middleware && \
    strip /concordium-client-bin
# Middleware and concordium-client is now built

FROM node:11 as node-build
WORKDIR /
RUN mkdir -p -m 0600 ~/.ssh && ssh-keyscan gitlab.com >> ~/.ssh/known_hosts
RUN --mount=type=ssh git clone --branch master git@gitlab.com:Concordium/node-dashboard.git
WORKDIR /node-dashboard
ENV NODE_ENV=development
# Building node dashboard
RUN npm i
RUN npm run build
# Node dashbaord built

FROM ubuntu:20.04
EXPOSE 8888
EXPOSE 10000
ENV RPC_SERVER_ADDR=0.0.0.0
ENV MODE=basic
ENV BOOTSTRAP_FIRST_NODE=bootstrap.eu.staging.concordium.com:8888
ENV DATA_DIR=/var/lib/concordium/data
ENV CONFIG_DIR=/var/lib/concordium/config
ENV EXTRA_ARGS="--no-dnssec"
ENV NODE_URL=localhost:10000
ENV COLLECTORD_URL=https://dashboard.eu.staging.concordium.com/nodes/post
ENV GRPC_HOST=http://localhost:10000
ENV DISTRIBUTION_CLIENT=true
ENV ENABLE_TERM_HANDLER=true
RUN apt-get update && apt-get install -y unbound curl netbase ca-certificates supervisor nginx libtinfo6 libpq-dev liblmdb-dev jq
COPY --from=build /build-project/p2p_client-cli /p2p_client-cli
COPY --from=build /build-project/node-collector /node-collector
COPY --from=build /build-project/start.sh /start.sh
COPY --from=build /build-project/genesis.dat /genesis.dat
RUN sha256sum /genesis.dat
COPY --from=haskell-build /libs/* /usr/lib/
COPY --from=haskell-build /middleware /middleware
COPY --from=haskell-build /concordium-client-bin /usr/local/bin/concordium-client
COPY --from=haskell-build /genesis-binaries /genesis-binaries
COPY --from=node-build /node-dashboard/dist/public /var/www/html/
RUN mkdir /var/www/html/public
RUN mv /var/www/html/*.js /var/www/html/public/
RUN sed -i 's/try_files.*$/try_files \$uri \/index.html =404;/g' /etc/nginx/sites-available/default
RUN ln -s /usr/lib/x86_64-linux-gnu/libtinfo.so.6.1 /usr/lib/x86_64-linux-gnu/libtinfo.so.5
COPY ./scripts/supervisord.conf /etc/supervisor/supervisord.conf
COPY ./scripts/concordium.conf /etc/supervisor/conf.d/concordium.conf
COPY ./scripts/staging-net-client.sh /staging-net-client.sh
ENTRYPOINT [ "/staging-net-client.sh" ]
