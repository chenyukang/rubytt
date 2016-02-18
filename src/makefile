TARGET = main

SOURCES = \
	node.ml \
	read_json.ml \
	main.ml

OCAMLBUILD = corebuild -use-ocamlfind -pkg yojson -pkg alcotest -pkg ounit -pkg str

default: byte

all: byte native test dot

byte:
	$(OCAMLBUILD) $(TARGET).byte; cp $(TARGET).byte ../bin;

native:
	$(OCAMLBUILD) $(TARGET).native; cp $(TARGET).native ../bin;

test:
	$(OCAMLBUILD) test.native; ./test.native

update:
	$(OCAMLBUILD) update_test.byte; ./update_test.byte

dot:
	$(OCAMLBUILD) proj.docdir/dep.dot;
  # remove the line of "rotate=90"
	grep -vwE "(rotate)" _build/proj.docdir/dep.dot > dep.dot;
	dot dep.dot -Tpng -o dep.png;

clean:
	$(OCAMLBUILD) -clean
