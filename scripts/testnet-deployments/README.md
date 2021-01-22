# Testnet deployments

Using the `build-all-docker.sh` script, 5 images are built.

|                   | concordium-node    | p2p_bootstrapper-cli | node-collector     | node-collector-backend | genesis-data       | start.sh           |
| ----------------- | ---------------    | -------------------- | --------------     | ---------------------- | ------------       | --------           |
| universal         | :heavy_check_mark: | :heavy_check_mark:   | :heavy_check_mark: | :heavy_check_mark:     | :heavy_check_mark: | :heavy_check_mark: |
| client            | :heavy_check_mark: |                      |                    |                        | :heavy_check_mark: | :heavy_check_mark: |
| bootstrapper      |                    | :heavy_check_mark:   |                    |                        |                    | :heavy_check_mark: |
| collector-backend |                    |                      | :heavy_check_mark: |                        |                    | :heavy_check_mark: |
| collector         |                    |                      |                    | :heavy_check_mark:     |                    | :heavy_check_mark: |

The images can be built in debug or release mode and use haskell profiling libraries or not, specified by the arguments to `build-all-docker.sh`:
```
./build-all-docker.sh VERSION-TAG [debug|release] [profiling:[true|false]]
```
