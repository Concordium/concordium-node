#!/bin/sh
( 
  git clone git@gitlab.com:Concordium/oak/oak-compiler.git oak-compiler &&
  cd oak-compiler &&
  git checkout f80840f171ffce63b85b1744f87490b9eb8eb70a )
