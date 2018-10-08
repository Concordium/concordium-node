FROM concordium/build:latest
WORKDIR /build-project
COPY ./scripts/local-start-bootstrapper.sh ./start-bootstrapper.sh
RUN chmod +x ./start-bootstrapper.sh
EXPOSE 8888
RUN pacman -Syy heaptrack --noconfirm
ENV NODE_ID="2000000000000000000000000000000000000000000000000000000000000000"
ENTRYPOINT ./start-bootstrapper.sh