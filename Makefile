sources = $(wildcard lib/*.ml lib/*.mli bin/*.ml bin/*.mli)

build: $(sources) Makefile
	dune build

deps:
	dune build || true
	opam install . --deps-only

$(HOME)/.opam/default/bin/dune:
	opam install dune

.PHONEY: build deps
