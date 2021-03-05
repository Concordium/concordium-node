# syntax=docker/dockerfile:experimental
FROM 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/static-libraries:0.18

ENV GHC_VERSION 8.10.4

COPY scripts/static-libraries/build-static-libraries.sh /build-static-libraries.sh
COPY scripts/static-libraries/build-static-libraries-copy-out.sh /build-static-libraries-copy-out.sh
COPY . /build

RUN chmod +x /build-static-libraries.sh
WORKDIR /
RUN --mount=type=ssh ./build-static-libraries.sh
ENTRYPOINT ["./build-static-libraries-copy-out.sh"]
