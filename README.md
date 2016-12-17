# Rubytt

[![Build Status](https://travis-ci.org/chenyukang/rubytt.svg?branch=master)](https://travis-ci.org/chenyukang/rubytt)

Rubytt is a static Ruby code analyzer, optimized especially for Rails app.

1. The basic ideas comes from [rubysonar](https://github.com/yinwang0/rubysonar), but with more help from analysis of db/schema.rb,
we may get more accurate types for the code annotation or bug finding.

2. I also add some visualizations for the class/db/model.

3. Check the unused variable, this may reported out some bugs, [ruby-lint](https://github.com/YorickPeterse/ruby-lint) seems can report out these errors, but it's too slow.

4. Check the undefined error [TODO]

5. Check the type mismatch error  [TODO]

6. The project is in experimental status. It's written in OCaml.


to be finished ....

Compile it:

```shell
gem install parallel ruby-progressbar

brew install opam
opam init -y
eval `opam config env`
opam update
opam switch 4.02.1
opam install core yojson alcotest ounit stringext -y

brew install graphviz ## ignore it if installed

make  ## or use: make native
```

Usage:

```shell
./main.byte -h   ## see the help messages
./main.byte -s source_dir -t type -o res       ## analysis type for source_dir, dump html in res directory
./main.byte -s source_dir -t class -o res.png  ## analysis class for source_dir, dump out result to res.png
./main.byte -s source_dir -t db -o res.png     ## analysis db for source_dir, dump out result to res.png
./main.byte -s source_dir -t model -o res.png  ## analysis model for source_dir, dump out result to res.png
./main.byte -s source_dir -t check             ## try find unused variable bugs
```
