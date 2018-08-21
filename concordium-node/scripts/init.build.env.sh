#!/bin/sh
git clone https://github.com/mitls/hacl-c
( cd hacl-c && make && cp libhacl.so /usr/lib );
rm -rf hacl-c

git clone https://git.nlnetlabs.nl/ldns
( cd ldns && git submodule update --init && libtoolize -ci && autoreconf -fi && ./configure --prefix=/usr && make && make install )
rm -rf ldns
ldconfig
rustup default nightly

