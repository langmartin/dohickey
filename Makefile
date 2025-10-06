static = static/js_client.js static/js_service_worker.js

build_all: build static/js_client static/js_service_worker

build:
	touch $(static)
	dune build

# https://discuss.ocaml.org/t/js-of-ocaml-output-performs-considerably-worse-when-built-with-profile-release-flag/8862/15
release: $(sources) Makefile
	touch $(static)
	dune build --profile=release

release_all: release static/js_client static/js_service_worker

static/%:
	cp -f _build/default/src/$*/main.bc.js static/$*.js

deps:
	dune build || true
	opam install . --deps-only

user:
	opam install -y merlin ocp-indent utop
	opam user-setup install

$(HOME)/.opam/default/bin/dune:
	opam install dune

.PHONEY: build_all build deps user release release_all
