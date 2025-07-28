(lang dune 3.17)

(name dohickey)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license "AGPL-3.0-or-later")

(documentation https://url/to/documentation)

(package
 (name dohickey)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends alcotest
          base64
          brr
          dream
          js_of_ocaml
          js_of_ocaml-ppx
          ppx_blob
          sqlite3
          yojson
          dune
          ocaml
          unix)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
