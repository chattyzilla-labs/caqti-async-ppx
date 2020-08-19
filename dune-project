(lang dune 2.0.1)

(name ppx_rapper)

(version 2.0.0)

(source
 (github chattyzilla-labs/caqti-async-ppx))

(documentation "https://github.com/chattyzilla-labs/caqti-async-ppx")

(license MIT)


(maintainers "Dakota Murphy <dakota@chattyzilla.com>")

(generate_opam_files true)

(package
 (name ppx_rapper)
 (synopsis "Syntax extension for Caqti/PostgreSQL queries")
 (depends
  (ocaml
   (>= 4.07))
  (dune
   (>= 2.0.1))
  pg_query
  ppxlib
  base
  caqti
  caqti-async
  (caqti-type-calendar :with-test)))