# syntax=docker/dockerfile:experimental
FROM concordium/static-libraries:0.20

ENV GHC_VERSION 8.10.4

COPY scripts/static-libraries/build-static-libraries.sh /build-static-libraries.sh
COPY scripts/static-libraries/build-static-libraries-copy-out.sh /build-static-libraries-copy-out.sh
RUN mkdir /build
COPY LICENSE /build/LICENSE
COPY concordium-base /build/concordium-base
COPY concordium-consensus /build/concordium-consensus


RUN chmod +x /build-static-libraries.sh
WORKDIR /
RUN ./build-static-libraries.sh
ENTRYPOINT ["./build-static-libraries-copy-out.sh"]
