# rubytt

to be finished ....

Compile it:

```shell
brew install ocaml 
brew install opam
opam install core yojson alcotest ounit

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
