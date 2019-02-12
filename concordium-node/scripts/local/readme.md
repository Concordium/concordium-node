# Local test based in Docker-Compose

This docker compose deploy a local test based on the following structure:
 * 1 *bootstrapper* node.
 * 1 *testrunner* node.
 * 1..N *baker* nodes. By default, it launches just one. See *RUN* section to
   know how to using several replicas.
 * 1 *baker_id_gen* node. This service is just to generate sequential _ID s_
   which will be use by each *baker* node.

All services use the same docker image: *concordium/common:latest*. However,
each service defined in `docker-compose.yml` overwrites its `entrypoint` to run
specific launcher script.

## Advantages

### No external port or IP is needed
This docker-compose generates a network where nodes are deployed, so it is not
needed to use *external ports or ips*.

### Right start-up order
Start up order is defined by `depends_on` instruction. It defines what other
services should be running _before_ start the current service. In this case, any
_node_ (baker) requires that `bootstrapper` and `baker_id_gen` have to be
running.

### Automatic link to bootstrapper

Each baker node uses `BOOTSTRAP_NODE` environment variable to link with the
`bootstrapper` node. It uses the name of the service instead of its IP address.

### Common log for all services

Using docker-compose approach, you can see logs from all nodes together.

## How to build

Using `docker-compose` `build` command as usual:

```bash
$> docker-compose build
```

## Run with N baker nodes 

Use `--scale` argument to increase number of nodes for a specific service. As
example, to run it using 4 baker nodes:

```bash
$> docker-compose up --scale node=4
```

In order to *stop* and *remove* containers, networks, volumes, etc., you have to
use:

```
$> docker-compose down
```
