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
./main.byte -s source_directory -t type -o res  ## anaysis type for source_directory, dump html in res
```
