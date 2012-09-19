#!/usr/bin/env bash

cd gc-7.2
CC='gcc -m32' ./configure --prefix=$1 --disable-threads --target=i386-unknown-linux-gnu
make 
make install
