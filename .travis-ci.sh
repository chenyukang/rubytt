# Edit this for your own project dependencies
OPAM_DEPENDS="core yojson alcotest ounit stringext"

case "$OCAML_VERSION,$OPAM_VERSION" in
    4.02.1,1.2.0) ppa=avsm/ocaml41+opam10 ;;
    4.00.1,1.1.0) ppa=avsm/ocaml41+opam11 ;;
    *) echo Unknown $OCAML_VERSION,$OPAM_VERSION; exit 1 ;;
esac

echo "yes" | sudo add-apt-repository ppa:$ppa
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam
export OPAMYES=1
opam init
opam install ${OPAM_DEPENDS}
eval `opam config env`
make
make test
