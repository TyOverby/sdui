# TyOverby/sdui

`sdui` is a frontend for the [AUTOMATIC1111 Stable Diffusion API](https://github.com/AUTOMATIC1111/stable-diffusion-webui) 
with a focus on:

- displaying intermediate results
- a built-in gallery 
- tools for managing prompts and parameters


## Building

```bash
git clone git@github.com:TyOverby/sdui.git
cd sdui

opam switch create ./ 4.14.1

opam repo add janestreet-bleeding https://ocaml.janestreet.com/opam-repository
opam repo add janestreet-bleeding-external https://github.com/janestreet/opam-repository.git#external-packages
opam update

opam install bonsai yojson ppx_yojson_conv
# For development:
# opam install ocamlformat merlin

dune build @check bin/main.bc.js bin/index.html --profile release
# For development: 
# dune build @check bin/main.bc.js bin/index.html -w

$(cd _build/default/bin && python3 -m http.server)
```

# Screenshots
## Dark Mode
<img width="1019" alt="image" src="https://github.com/TyOverby/sdui/assets/573215/30ef1f2e-7ee1-49d3-bb1e-e1cfa941ceb3">

## Light Mode
<img width="1019" alt="image" src="https://github.com/TyOverby/sdui/assets/573215/d2b4431d-841e-4770-86d9-e746ed37e8a9">
