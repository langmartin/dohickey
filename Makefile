sources = $(wildcard lib/*.ml lib/*.mli bin/*.ml bin/*.mli)

build: $(sources) Makefile
	dune build

deps:
	opam install dune
	make
	opam install . --deps-only

.PHONEY: build deps
