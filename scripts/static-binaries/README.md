# Building mostly static binaries for linux

Due to the complexity of Haskell dependencies it is hard to link a node
dynamically in a format suitable for distribution. The script and Dockerfile in
this directory can be used to build mostly statically linked binaries for
ubuntu. Small modifications of the build scripts will yield binaries for similar
platforms. The base docker image for the build should be varied and the
specific package manager for installing dependencies (and possibly renaming dependencies).

## [build-static-binaries.sh](./build-static-binaries.sh)
    This is the main script that should be used. **It is intended to be run from
    the root of the repository.** The script supports the following environment variables
    - `UBUNTU_VERSION`, numeric tag, 20.04, 18.04
    - `STATIC_LIBRARIES_IMAGE_TAG`, tag of the docker image used to build static
      libraries. `latest` should work, but otherwise see which tags are
      available on dockerhub.
    - `GHC_VERSION` which Haskell compiler version to use. Generally this should
      match whatever the version is specified by the resolver in [stack.yaml](../../concordium-consensus/stack.yaml).
    - `EXTRA_FEATURES` (optional) extra features that will be used when building
      the node, e.g., `"collector,profiling"`. If omitted it defaults to no
      features apart from `static`.

   The outcome of running the script is a docker image called
   `static-node-binaries` which contains all the built binaries in `/build/bin`
   directory. The needed ones can be copied to the host system by running, e.g.,

   ```console
   $ docker run -v $(pwd)/out:/out static-node-binaries cp /build/bin/concordium-node /out
   ```

## [build-on-ubuntu.sh](./build-on-ubuntu.sh)
   Script used to install dependencies and build the node in a fresh ubuntu
   instance.

## [static-binaries.Dockerfile](./static-binaries.Dockerfile)
   The dockerfile used to build the binaries.

# Opportunities for improvement

- Build other binaries as well, e.g., genesis tool, database exporter.
- Improve caching of the layers. Currently too many things are copied into the
  different stages which makes caching less than ideal (irrelevant changes cause
  rebuilds).
