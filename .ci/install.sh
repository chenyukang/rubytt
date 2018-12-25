#!/bin/bash

set -e

if [[ "$TRAVIS_OS_NAME" == "osx" ]]; then
    brew install opam
    brew install ruby
    gem install ruby-progressbar
fi

if [[ "$TRAVIS_OS_NAME" == "linux" ]]; then
    echo "y" | sudo apt-get install --force-yes ocaml ocaml-native-compilers camlp4-extra opam rubygems
    sudo apt-get install -y ruby-dev
    sudo gem install ruby-progressbar
fi

opam init -y
eval `opam config env`
opam update
opam switch create 4.06.1
opam install core yojson alcotest ounit stringext -y
