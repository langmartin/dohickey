sources = $(wildcard lib/*.ml* lib/*/*.ml* bin/*.ml js_*/*.ml*)

build_static: build static/js_client static/js_service_worker

build: $(sources) Makefile
	dune build

static/%: build
	cp -f _build/default/$*/main.bc.js static/$*.js

deps:
	dune build || true
	opam install . --deps-only

$(HOME)/.opam/default/bin/dune:
	opam install dune

.PHONEY: build deps
