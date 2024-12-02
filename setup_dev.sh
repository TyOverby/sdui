#!/bin/bash

curl -fsSL https://opam.ocaml.org/install.sh > install_opam.sh
chmod +x install_opam.sh
./install_opam.sh --download-only

./opam init
./opam update

./opam repo add with-extensions https://github.com/janestreet/opam-repository.git#with-extensions
./opam switch create ./ 5.2.0+flambda2 --repos with-extensions,default --empty
eval $(./opam env)

./opam install ./ \
 ocamlformat.0.26.2+jst \
 merlin.5.2.1-502+jst \
 ocaml-lsp-server.1.19.0+jst
