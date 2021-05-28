# Distribution scripts

The scripts `build-and-push-*.sh` distribution create images that contain
- `concordium-node`
- `node-collector`
- the specified genesis block.
- `node-dashboard` and supporiting backends (grpc proxy and nginx)

The node and all services are started by `supervisord` following the
configuration of `supervisord.conf` and `concordium.conf`.
