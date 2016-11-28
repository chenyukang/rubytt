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

cd rubytt/src;
make 
```

Usage:

```shell
./main.byte -h   ## see the help messages
./main.byte -s source_dir -t type -o result       ## anaysis type for source_dir, dump html in result directory
./main.byte -s source_dir -t class -o result.png  ## anaysis class for source_dir, dump out result to result.png
./main.byte -s source_dir -t db -o result.png     ## anaysis db for source_dir, dump out result to result.png
./main.byte -s source_dir -t model -o result.png  ## anaysis model for source_dir, dump out result to result.png
```
