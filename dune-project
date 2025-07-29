(lang dune 3.19)

(name ksu)

(generate_opam_files true)

(source
 (github rureirureirurei/ksu)
)

(authors "Denys Zinoviev")

(package
 (name ksu)
 (depends
  ocaml
  menhir
  dune
 )
)
