sources = $(wildcard lib/*.ml lib/*.mli bin/*.ml bin/*.mli)

build: $(sources) Makefile
	dune build

deps:
	dune build || true
	opam install dune
	opam install . --deps-only

.PHONEY: build deps
