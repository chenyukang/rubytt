#!/bin/bash
set -e

opam switch create 4.06.1

eval `opam config env`

make && make test

