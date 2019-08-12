# Docker-Compose
For a local docker compose setup, a docker-compose.yml file has been provided in the root of this repository. It uses a image hosted in Docker hub built automatically upon push to the develop branch.

For the most simple and common setup, simply run
```
NUM_BAKERS=5 DESIRED_PEERS=4 docker-compose up --scale baker=5
```
in the repository root

For more complicated setups the EXTRA_ARGS environment variable can be set.