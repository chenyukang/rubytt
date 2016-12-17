TARGET = main

SRC=src

OCAMLBUILD = cd $(SRC); corebuild -use-ocamlfind -pkg yojson,alcotest,str,stringext -cflags -w,-45,-w,-11,-w,-27,-w,-26

default: byte

all: byte native test dot

prepare_dump:
	$(OCAMLBUILD) gen_dump.byte; ./gen_dump.byte;

byte: prepare_dump
	$(OCAMLBUILD) $(TARGET).byte; cp $(TARGET).byte ../bin;

native: prepare_dump
	$(OCAMLBUILD) $(TARGET).native; cp $(TARGET).native ../bin;

test:
	$(OCAMLBUILD) test.native;
	./src/test.native -e

update:
	$(OCAMLBUILD) test.native;
	./bin/test.native -u

dot:
	$(OCAMLBUILD) proj.docdir/dep.dot;
	# remove the line of "rotate=90"
	grep -vwE "(rotate)" ./src/_build/proj.docdir/dep.dot > dep.dot;
	cd src; dot dep.dot -Tpng -o dep.png

install:
	sudo cp ./bin/main.native /usr/local/bin/rubytt

clean:
	$(OCAMLBUILD) -clean
	rm -rf _tests
