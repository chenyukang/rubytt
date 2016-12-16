#!/bin/bash

echo -e "\e[31m=== Running $0 ===\e[0m"

set -e

if [[ "$TRAVIS_OS_NAME" == "osx" ]]; then
    brew update;
fi

if [[ "$TRAVIS_OS_NAME" == "linux" ]]; then
    sudo add-apt-repository --yes ppa:avsm/ppa
    sudo apt-get install --yes ocaml ocaml-native-compilers camlp4-extra opam
fi
