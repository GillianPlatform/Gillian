OCAML_VARIANT=ocaml-variants.5.3.0+options
DEV_PACKAGES=ocaml-lsp-server,feather,fileutils
BUILD_PACKAGES=ocamlformat.0.27.0,odoc
ifeq ($(NO_OPAM_EXEC),)
	OPAM_EXEC = opam exec --
else
	OPAM_EXEC =
endif

build:
	$(OPAM_EXEC) dune build @all

fmt:
	$(OPAM_EXEC) dune fmt

deps:
	$(OPAM_EXEC) dune build gillian.opam wisl.opam gillian-js.opam gillian-c.opam gillian-c2.opam transformers.opam
	opam install . -y --deps-only

init-dev:
	opam switch create . --packages=${OCAML_VARIANT},${BUILD_PACKAGES},${DEV_PACKAGES} -y --deps-only
	$(OPAM_EXEC) ./githooks/install.ml

init-ci:
	opam install . -y --deps-only
	opam install $(shell echo ${BUILD_PACKAGES} | tr ',' ' ') -y

uninstall:
	opam remove gillian gillian-c gillian-js wisl gillian-c2 transformers -y

watch:
	$(OPAM_EXEC) dune build --watch

c-init-env:
	./Gillian-C/scripts/setup_environment.sh

wisl-init-env:
	./wisl/scripts/setup_environment.sh

js-init-env:
	./Gillian-JS/scripts/setup_environment.sh

docs:
	@echo "===== BUILDING ODOC ====="
	make odoc
	@echo "===== BUILDING SPHINX ====="
	make sphinx

docs-watch:
	./scripts/watch_docs.sh

odoc:
	$(OPAM_EXEC) dune build @doc
	mkdir -p _docs
	rsync -auv --delete _build/default/_doc/_html/. _docs/odoc/

odoc-watch:
	./scripts/watch_odoc.sh > /dev/null &
	$(OPAM_EXEC) dune build @doc --watch --terminal-persistence=preserve

sphinx:
	sphinx-build sphinx _docs/sphinx

sphinx-watch:
	sphinx-autobuild sphinx _docs/sphinx/

.PHONY: init-dev watch docs build c-init-env wisl-init-env js-init-env docs odoc sphinx
