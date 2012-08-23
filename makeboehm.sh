#!/usr/bin/env bash

cd gc-7.2
./configure --prefix=$1 --disable-threads
make 
make install
