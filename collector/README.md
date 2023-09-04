## Collector

To allow the network dashboard to display nodes in the network and their current status, a node must run a collector, which is a process that uses the GRPC api of the node to collect information and sends it to a centralized collector backend.

See [./collector-backend/](../collector-backend/) for details of the collector backend and how to run it.

Assuming you have a node running locally with GRPC V2 available at `127.0.0.1:20000` and a collector backend at `127.0.0.1:8080`, a collector can be run using:

```console
$> cargo run -- --collector-url http://127.0.0.1:8080/post/nodes --grpc-host http://127.0.0.1:20000  --node-name "my-node-name"
```

### TLS
If the node is configured to be using TLS connections then one has to configure the collector (the `--grpc-host` flag) to be using the URL of the domain of which the certificate was issued for.
