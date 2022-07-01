* Docker distribution image

This directory contains both a [Dockerfile](./builder.Dockerfile) for building
the distribution image for different environments, as well as two sample
configuration files for running mainnet/testnet nodes.

The builder is used by a [jenkins job](../../../jenkinsfiles/distribution-image.Jenkinsfile) to build and publish
the official distribution images on dockerhub.

The image may also be built directly by using
```shell
GHC_VERSION=9.0.2 STATIC_LIBRARIES_IMAGE_TAG=latest UBUNTU_VERSION=20.04 ./scripts/distribution/docker/build-distribution-image.sh
```
Since the build needs to clone the genesis data repository you may need to
modify the script a bit to specify the default `ssh` credentials by setting the
path to the keys file, changing

```shell
   --ssh default
```
to
```shell
   --ssh default=/path/to/private/ssh/keys/file
```
