# Rubytt

[![Build Status](https://travis-ci.org/chenyukang/rubytt.svg?branch=master)](https://travis-ci.org/chenyukang/rubytt)

Synopsis
===========

Rubytt is a static Ruby code analyzer, optimized especially for Rails app.

1. The basic ideas comes from [rubysonar](https://github.com/yinwang0/rubysonar), but with more help from analysis of db/schema.rb,
we may get more accurate types for the code annotation or bug finding.

2. Do some visualizations for the class/db/model.

3. Check the unused variable, this may reported out some bugs, [ruby-lint](https://github.com/YorickPeterse/ruby-lint) seems can report out these errors, but it's too slow.

4. Check the undefined error [PARTLY FINISHED]

5. Check the type mismatch error  [TODO]

6. The project is in experimental status. It's written in OCaml.


to be finished ....

For Users
---------
## Install

rubytt can be installed using OCaml package manager:

#### Linux

```
gem install parallel ruby-progressbar
sudo apt-get install --force-yes ocaml ocaml-native-compilers camlp4-extra opam
opam init
eval `opam config env`
opam install rubytt
```

#### Mac
```
gem install parallel ruby-progressbar
brew install opam
opam init
eval `opam config env`
opam install rubytt
```

## Usage

```shell
rubytt -h   ## see the help messages
rubytt -s source_dir -t type -o res       ## analysis type for source_dir, dump html in res directory
rubytt -s source_dir -t class -o res.png  ## analysis class for source_dir, dump out result to res.png
rubytt -s source_dir -t db -o res.png     ## analysis db for source_dir, dump out result to res.png
rubytt -s source_dir -t model -o res.png  ## analysis model for source_dir, dump out result to res.png
rubytt -s source_dir -t check             ## try find unused or undef variable bugs
```

---------------------------------------
## How to compile:

```shell
gem install parallel ruby-progressbar

// Linux:
sudo apt-get install --force-yes ocaml ocaml-native-compilers camlp4-extra opam

// Mac:
brew install opam

opam init -y
opam update
opam switch 4.02.1
opam install core yojson alcotest ounit stringext -y
eval `opam config env`

brew install graphviz ## ignore it if installed

make  ## or use: make native
```
