ARG base_image_tag
ARG ubuntu_version
FROM concordium/base:${base_image_tag} as build
WORKDIR /build
COPY ./concordium-base ./concordium-base
COPY ./concordium-consensus ./concordium-consensus
COPY ./LICENSE ./README.md ./
RUN stack build \
	--stack-yaml=./concordium-consensus/stack.yaml \
	--flag=concordium-consensus:-dynamic \
	--copy-bins \
	--local-bin-path=. \
	concordium-consensus:database-exporter

FROM ubuntu:${ubuntu_version}
RUN apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y jq awscli liblmdb0 && \
    rm -rf /var/lib/apt/lists/*
COPY --from=build /build/database-exporter /usr/local/bin/database-exporter
COPY --from=build /build/concordium-base/rust-src/target/release/*.so /usr/local/lib/
COPY --from=build /build/concordium-base/smart-contracts/wasm-chain-integration/target/release/*.so /usr/local/lib/
# Reconfigure dynamic linker to ensure that the shared libraries get loaded.
RUN ldconfig
COPY ./scripts/database-exporter/publish.sh /docker-entrypoint-publish.sh
ENTRYPOINT [ "/docker-entrypoint-publish.sh" ]
