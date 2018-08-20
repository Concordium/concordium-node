#!/bin/sh
echo "deb http://ftp.debian.org/debian stretch-backports main" >> /etc/apt/sources.list
apt-get update -yqq
apt-get -t stretch-backports install -yqq --no-install-recommends build-essential
apt-get -t stretch-backports install -y cmake golang-go libclang-dev clang libc6-dev
git clone https://github.com/mitls/hacl-c
( cd hacl-c && make && cp libhacl.so /usr/lib );
rm -rf hacl-c
wget https://github.com/google/protobuf/archive/v3.5.1.tar.gz
tar xzf v3.5.1.tar.gz
( cd protobuf-3.5.1 && autoreconf -i &&  ./configure --prefix=/usr && make && make install )
rm -rf protobuf-3.5.1
git clone https://git.nlnetlabs.nl/ldns
( cd ldns && git submodule update --init && libtoolize -ci && autoreconf -fi && ./configure --prefix=/usr && make && make install )
rm -rf ldns
ldconfig
rustup default nightly
