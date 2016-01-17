TARGET = main

SOURCES = \
	node.ml \
	read_json.ml \
	main.ml


OCAMLBUILD = corebuild -pkg yojson -pkg alcotest -pkg ounit

default: byte

all: byte native test

byte:
	$(OCAMLBUILD) $(TARGET).byte

native:
	$(OCAMLBUILD) $(TARGET).native

test:
	$(OCAMLBUILD) test.byte; ./test.byte

clean:
	$(OCAMLBUILD) -clean
