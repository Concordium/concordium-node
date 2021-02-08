# Development image for local deployment

This folder contains a `Dockerfile` for building a multi-component image that is well suited for
spinning up a network using docker-compose for local development.

## Usage

Two parameterized `docker-compose` files are available:

- `bakers.yaml`: Run a network of bakers with collectors and a collector-backend.
  This is useful for e.g. testing the network dashboard.
  A middleware instance needs to be started separately.
- `bakers+wallet-proxy.yaml`: Same as the above but also with a wallet-proxy instance running.
  A postgres instance is started as well and the nodes configured to ingest data.
  At the time of this writing, this setup seems outdated and broken and will be fixed ASAP.

It seems like there was an option that included a Middleware instance in the past.
Including this will be attempted once the Wallet Proxy setup has been fixed.

### Example

To boot a cluster of 5 nodes (no wallet-proxy), use the command

```
NUM_BAKERS=5 DESIRED_PEERS=4 docker-compose -f bakers.yaml up --scale baker=5 --force-recreate
```

Update the used docker image using 

```
NUM_BAKERS=5 DESIRED_PEERS=4 docker-compose -f docker-compose.yaml pull
```
...

## Build

See [dev-master.Jenkinsfile](https://gitlab.com/Concordium/concordium-node/-/blob/master/jenkinsfiles/dev-master.Jenkinsfile).

# OLD

Information from the old README that might still be relevant:
 
> Remember to clean out PostgreSQL data between runs using
> ```bash
> $ NUM_BAKERS=5 DESIRED_PEERS=4 docker-compose -f develop.wallet-dev.yml down
> ```
> 
> ## Delay baker startup if PostgreSQL starts too slowly
> If PostgreSQL starts too slowly the baker enabled for logging to it can be delayed by using the variable `DB_SLEEP` (the wallet-proxy has a default value of 30 set to delay start until PostgreSQL is up).
