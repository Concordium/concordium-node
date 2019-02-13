#!/bin/bash

set -e

BUILD_ENV=$(docker images | grep windowsbuildenv | wc -l)

if [ "$BUILD_ENV" -eq "0" ]
then
    echo "Building Windows Build Environment image. Please wait"
    docker build -f scripts/WindowsBuildEnv.Dockerfile -t concordium/windowsbuildenv . --no-cache
fi

BUILD=$(docker images | grep windows | wc -l)
#Should have one if only the environment exists. If it has two, then the build was already done. If it was zero then previous build failed and we should bail
if [ "$BUILD" -eq "0" ]
then
    echo "Build must have failed. Bailing out!"
    exit 1
elif [ "$BUILD" -eq "1" ]
then
    echo "Building Windows binary. Please wait"
    docker build -f scripts/Windows.Dockerfile -t concordium/windowsbuild . --no-cache
fi

# We should now have built the binaries. Let us extract them
rm -rf win_build
mkdir -p win_build
docker run --rm -v $(pwd)/win_build:/target concordium/windowsbuild /bin/bash -c "cp target/x86_64-pc-windows-gnu/debug/*.exe /target/"
cp deps/windows/vendor/* win_build/
cp deps/windows/libunbound/lib/*.dll win_build/
cp deps/windows/openssl-1.0.2h-win64-mingw/*.dll win_build/
cp deps/windows/HSdll.dll win_build/

echo "Build should now be done and files can be found in the win_build folder!"