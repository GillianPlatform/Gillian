OCAML_VARIANT=ocaml-variants.5.2.0+options
DEV_PACKAGES=ocaml-lsp-server,feather,fileutils
BUILD_PACKAGES=ocamlformat.0.26.2,odoc

build:
	dune build @all

deps:
	opam exec -- dune build gillian.opam wisl.opam gillian-js.opam gillian-c.opam kanillian.opam
	opam install . -y --deps-only

init-dev:
	opam switch create . --packages=${OCAML_VARIANT},${BUILD_PACKAGES},${DEV_PACKAGES} -y --deps-only
	opam exec -- ./githooks/install.ml

init-ci:
	opam install . -y --deps-only
	opam install $(shell echo ${BUILD_PACKAGES} | tr ',' ' ') -y

uninstall:
	opam remove gillian gillian-c gillian-js wisl kanillian -y

docs:
	@echo "===== BUILDING ODOC ====="
	opam exec -- dune build @doc
	mkdir -p _docs
	rsync -auv --delete _build/default/_doc/_html/. _docs/odoc/
	@echo "===== BUILDING SPHINX ====="
	sphinx-build sphinx _docs/sphinx

watch:
	opam exec -- dune build --watch

c-init-env:
	./Gillian-C/scripts/setup_environment.sh

wisl-init-env:
	./wisl/scripts/setup_environment.sh

js-init-env:
	./Gillian-JS/scripts/setup_environment.sh


.PHONY: init-dev watch docs build c-init-env wisl-init-env js-init-env docs