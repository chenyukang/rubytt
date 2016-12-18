#!/bin/bash
set -e

opam switch 4.02.1

eval `opam config env`

make test
