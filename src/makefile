TARGET = main

SOURCES = \
	node.ml \
	read_json.ml \
	main.ml

OCAMLBUILD = corebuild -use-ocamlfind -pkg yojson -pkg alcotest -pkg ounit -pkg str -pkg stringext -cflags -w,-45,-w,-11,-w,-27,-w,-26

default: byte

all: byte native test dot

prepare_dump:
	$(OCAMLBUILD) gen_dump.byte; ./gen_dump.byte;

byte: prepare_dump
	$(OCAMLBUILD) $(TARGET).byte; cp $(TARGET).byte ../bin;

native: prepare_dump
	$(OCAMLBUILD) $(TARGET).native; cp $(TARGET).native ../bin;

test:
	$(OCAMLBUILD) test.native; ./test.native -e

update:
	$(OCAMLBUILD) test.native; ./test.native -u

dot:
	$(OCAMLBUILD) proj.docdir/dep.dot;
    # remove the line of "rotate=90"
	grep -vwE "(rotate)" _build/proj.docdir/dep.dot > dep.dot;
	dot dep.dot -Tpng -o dep.png; open dep.png

clean:
	$(OCAMLBUILD) -clean
	rm -rf _tests
