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
	ocamldep *.ml | ../bin/ocamldot > dep.dot
	dot -Tpng dep.dot > dep.png
clean:
	$(OCAMLBUILD) -clean
