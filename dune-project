(lang dune 3.15)

(name dohickey)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name dohickey)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml
          dune
          yojson
          dream
          alcotest
          sqlite3
          ppx_blob)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
