(lang dune 3.19)

(name ccc)

(generate_opam_files true)

(source
 (github rureirureirurei/ccc)
)

(authors "Denys Zinoviev")

(package
 (name ccc)
 (depends
  ocaml
  menhir
  dune
 )
)
