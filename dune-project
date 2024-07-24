(lang dune 3.7)

(name irmin_effects_demo)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name irmin_effects_demo)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends 
   ocaml
   dune
   lwt
   lwt_ppx
   lwt_eio
   eio_main
   asai
   irmin
   irmin-fs)
 (tags
  (topics "to describe" your project)))
