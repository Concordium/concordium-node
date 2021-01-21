# Docker-Compose
## Building docker images
To build the stable image built in a Jenkins pipeline (it gets tagged `latest`, if not changed in the line shown below, so it matches the image hosted on docker-hub - and as the layers will have a newer version, it won't download from docker-hub unless the locally built image is removed via e.g. `docker image rmi ..`). It passes the local `ssh-agent` into the docker build environment for the needed stages to download internal crates with git directly. This image builds on `192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/base` so make sure to have either built this locally (check [devops:base-images/build-base.sh](https://gitlab.com/Concordium/devops/blob/master/base-images/build-base-docker.sh) for the syntax and current version), or have access to AWS ECR to pull it.

**These should be run from the [root](../) of the repository.**

```bash
$> git clone -b master --single-branch git@gitlab.com:Concordium/tools/baker_id_gen.git baker_id_gen # Only needed once, as it's a vital component to scaling the bakers inside docker-compose
$> scripts/download-genesis-data.sh
$> scripts/download-genesis-complementary-bundle.sh
$> echo $(cd deps/internal/consensus && git rev-parse HEAD) > CONSENSUS_VERSION
$> DOCKER_BUILDKIT=1 docker build -f scripts/dev-client.Dockerfile -t concordium/dev-client:latest --ssh default . --no-cache
```
## Latest stable from master branch
For a local docker compose setup, a docker-compose.yml file has been provided in the root of this repository. It uses a image hosted in Docker hub built automatically upon push to the master branch.

For the most simple and common setup, simply run the below command in the root of the checked out repository
```bash
$> NUM_BAKERS=5 DESIRED_PEERS=4 docker-compose up --scale baker=5 --force-recreate
```


For more complicated setups the EXTRA_ARGS environment variable can be set.

## Latest unstable from develop branch
For a local docker compose setup, a docker-compose.develop.yml file has been provided in the root of this repository. It uses a image hosted in Docker hub built automatically upon push to the develop branch.

For the most simple and common setup, simply run the below command in the root of the checked out repository
```bash
$> NUM_BAKERS=5 DESIRED_PEERS=4 docker-compose -f docker-compose.develop.yml up --scale baker=5 --force-recreate
```

## Latest debug from custom branch
For a local docker compose setup, a docker-compose.debug.yml file has been provided in the root of this repository. It uses a image hosted in Docker hub built manually from a specific branch. These builds must be considered extremely volatile!

For the most simple and common setup, simply run the below command in the root of the checked out repository
```bash
$> NUM_BAKERS=5 DESIRED_PEERS=4 docker-compose -f docker-compose.debug.yml up --scale baker=5 --force-recreate
```

## Latest debug from custom branch (with smart contract support)
For a local docker compose setup, a docker-compose.debug-sc.yml file has been provided in the root of this repository. It uses a image hosted in Docker hub built manually from a specific branch. These builds must be considered extremely volatile!

For the most simple and common setup, simply run the below command in the root of the checked out repository
```bash
$> NUM_BAKERS=5 DESIRED_PEERS=4 docker-compose -f docker-compose.debug-sc.yml up --scale baker=5 --force-recreate
```



For more complicated setups the EXTRA_ARGS environment variable can be set.

# Middleware local development mode
The PostGreSQL instance is exposed on port 5432/tcp and the username is `concordium`, password: `concordium`, and database name is `concordium`.

## Running the local development version from the stable master branch
Use docker-compose if you only need a middle-ware enabled set of nodes to test on
```bash
$> NUM_BAKERS=5 DESIRED_PEERS=4 docker-compose -f docker-compose.middleware.yml up --scale baker=5 --force-recreate
```

Remember to clean out PostGreSQL data between runs using
```bash
$> NUM_BAKERS=5 DESIRED_PEERS=4 docker-compose -f docker-compose.middleware.yml down
```

## Running the local development version from the unstable develop branch (middleware)
Use docker-compose if you only need a middle-ware enabled set of nodes to test on
```bash
$> NUM_BAKERS=5 DESIRED_PEERS=4 docker-compose -f docker-compose.develop.middleware.yml up --scale baker=5 --force-recreate
```

Remember to clean out PostGreSQL data between runs using
```bash
$> NUM_BAKERS=5 DESIRED_PEERS=4 docker-compose -f docker-compose.develop.middleware.yml down
```

## Delay baker startup if PostGreSQL starts too slowly
If PostGreSQL starts too slowly the baker enabled for logging to it can be delayed by using the variable `DB_SLEEP`


# Wallet local development mode
The PostGreSQL instance is exposed on port 5432/tcp and the username is `concordium`, password: `concordium`, and database name is `concordium`.
The wallet-proxy is mapped on port 14000/tcp.

## Running the local development version from the stable master branch
Use docker-compose if you only need a middle-ware enabled set of nodes to test on
```bash
$> NUM_BAKERS=5 DESIRED_PEERS=4 docker-compose -f docker-compose.wallet-dev.yml up --scale baker=5 --force-recreate
```

Remember to clean out PostGreSQL data between runs using
```bash
$> NUM_BAKERS=5 DESIRED_PEERS=4 docker-compose -f docker-compose.wallet-dev.yml down
```

## Running the local development version from the unstable develop branch
Use docker-compose if you only need a middle-ware enabled set of nodes to test on
```bash
$> NUM_BAKERS=5 DESIRED_PEERS=4 docker-compose -f docker-compose.develop.wallet-dev.yml up --scale baker=5 --force-recreate
```

Remember to clean out PostGreSQL data between runs using
```bash
$> NUM_BAKERS=5 DESIRED_PEERS=4 docker-compose -f docker-compose.develop.wallet-dev.yml down
```

## Delay baker startup if PostGreSQL starts too slowly
If PostGreSQL starts too slowly the baker enabled for logging to it can be delayed by using the variable `DB_SLEEP` (the wallet-proxy has a default value of 30 set to delay start until PostGreSQL is up).
