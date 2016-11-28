# Rubytt

Rubytt is an static Ruby code analyzer, especially for Rails app. 

1. The basic ideas comes from [rubysonar](https://github.com/yinwang0/rubysonar), but with more the help of analyis on db/schema.rb, 
we may get more types for the code annotation. 

2. I also add some visualizations for the class/db/model.

3. Check the unused variable, this may reported out some bugs, [ruby-lint](https://github.com/YorickPeterse/ruby-lint) seems can report out these errors, but it's too slow.

4. The project is in experimental status. It's written in OCaml

to be finished ....

Compile it:

```shell
brew install ocaml 
brew install opam
opam install core yojson alcotest ounit stringext
eval `opam config env`

brew install graphviz ## ignore it if installed

cd rubytt/src;
make  ## or use: make native 
```

Usage:

```shell
./main.byte -h   ## see the help messages
./main.byte -s source_dir -t type -o res       ## anaysis type for source_dir, dump html in re directory
./main.byte -s source_dir -t class -o res.png  ## anaysis class for source_dir, dump out result to res.png
./main.byte -s source_dir -t db -o res.png     ## anaysis db for source_dir, dump out result to res.png
./main.byte -s source_dir -t model -o res.png  ## anaysis model for source_dir, dump out result to res.png
./main.byte -s source_dir -t check             ## try find unused variable bugs
```
