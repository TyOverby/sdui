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
eval $(opam env)

opam repo add janestreet-bleeding https://ocaml.janestreet.com/opam-repository
opam repo add janestreet-bleeding-external https://github.com/janestreet/opam-repository.git#external-packages
opam update

opam install bonsai yojson ppx_yojson_conv
# For development:
# opam install ocamlformat merlin

dune build @check main.bc.js index.html --profile release
# For development: 
# dune build @check main.bc.js index.html -w

$(cd _build/default && python3 -m http.server)
```

# Screenshots
## Dark Mode
![image](https://github.com/TyOverby/sdui/assets/573215/7da96654-a9fd-41a4-9bc3-1e9e7c560a28)

## Light Mode
<img width="1019" alt="image" src="https://github.com/TyOverby/sdui/assets/573215/b84dc7f0-00ed-4a5e-ac1e-6a757eda81d9">

