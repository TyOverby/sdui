#!/bin/bash

set -euo pipefail

sudo apt install bubblewrap

curl -fsSL https://opam.ocaml.org/install.sh > install_opam.sh
chmod +x install_opam.sh
./install_opam.sh --download-only --version 2.3.0
mv opam-2.3.0-x86_64-linux ./opam
chmod +x opam

./opam init
./opam repo add with-extensions https://github.com/janestreet/opam-repository.git#with-extensions --dont-select
./opam update
./opam switch create 5.2.0+flambda2 --repos with-extensions,default
eval $(opam env --switch=5.2.0+flambda2)

./opam install --yes \
 ocamlformat.0.26.2+jst \
 merlin.5.2.1-502+jst \
 ocaml-lsp-server.1.19.0+jst \
 dune

./opam install --yes ./

eval $(opam env --switch=5.2.0+flambda2)

dune build bin/main.bc.js
