(lang dune 3.17)
;; (lang dune 3.7)

(name sdui)

(generate_opam_files true)

(source (github TyOverby/sdui))

(authors "Ty Overby")

(maintainers "Ty Overby")

(license LICENSE)

(documentation https://github.com/TyOverby/sdui)

(package
 (name sdui)
 (synopsis "A frontend for the 1111 stable-diffusion API")
 (description "TODO: write a longer description")
 (depends ocaml dune bonsai)
 (tags ())
 (allow_empty))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
