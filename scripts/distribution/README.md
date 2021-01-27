# Distribution scripts

The scripts `build-and-push-*-client.sh` create two images that contain the
`concordium-node`, the `node-collector` and the relevant `genesis-data` for 20
bakers (the initial 20 concordium-bakers).

Running the node is done by `supervisord` following the configuration of
`supervisord.conf` and `concordium.conf`.
