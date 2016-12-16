#!/bin/bash

set -e
eval $(opam config env)

make all
