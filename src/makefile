TARGET = main

SOURCES = \
	node.ml \
	read_json.ml \
	main.ml

OCAMLBUILD = corebuild -use-ocamlfind -pkg yojson -pkg alcotest -pkg ounit -pkg str

default: byte

all: byte native test

byte:
	$(OCAMLBUILD) $(TARGET).byte

native:
	$(OCAMLBUILD) $(TARGET).native

test:
	$(OCAMLBUILD) test.native; ./test.native

update:
	$(OCAMLBUILD) update_test.byte; ./update_test.byte

clean:
	$(OCAMLBUILD) -clean
