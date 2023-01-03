* Docker distribution image

This directory contains both a [Dockerfile](./builder.Dockerfile) for building
the distribution image for different environments, as well as two sample
configuration files for running mainnet/testnet nodes.

The builder is used by a [jenkins job](../../../jenkinsfiles/distribution-image.Jenkinsfile) to build and publish
the official distribution images on dockerhub.

The image may also be built directly by using
```shell
image_name=testnet-node\
image_tag=4.2.1\
environment=testnet\
genesis_ref=master\
genesis_path=testnet/2022-06-13/genesis_data\
ghc_version=9.2.5\
static_libraries_image_tag=latest\
./scripts/distribution/docker/build-distribution-image.sh
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
