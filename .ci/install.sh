#!/bin/bash

set -e

if [[ "$TRAVIS_OS_NAME" == "osx" ]]; then
    brew install opam
fi
if [[ "$TRAVIS_OS_NAME" == "linux" ]]; then
    sudo apt-get install --yes ruby-dev opam
    #sudo apt-get install --yes ocaml ocaml-native-compilers camlp4-extra opam
fi

opam init
eval `opam config env`
opam update
opam switch 4.02.1
opam install core yojson alcotest ounit stringext -y
