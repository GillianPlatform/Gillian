
OCAML_VARIANT=ocaml-variants.5.2.0+options
DEV_PACKAGES=ocaml-lsp-server,feather,fileutils
BUILD_PACKAGES=ocamlformat.0.26.2,odoc

build:
	dune build @all

init-dev:
	opam switch create . --packages=${OCAML_VARIANT},${BUILD_PACKAGES},${DEV_PACKAGES} -y --deps-only
	opam exec -- ./githooks/install.ml
	
init-ci:
	opam install . -y --deps-only
	opam install $(shell echo ${BUILD_PACKAGES} | tr ',' ' ') -y

dist:
	opam exec -- dune build @install
	rm -rf _dist
	mkdir _dist _dist/bin _dist/lib
	cp -r _opam/share _dist/
	cp -r _opam/lib/stublibs _dist/lib/
	cp _opam/bin/wisl _opam/bin/gillian-c _opam/bin/gillian-js _opam/bin/kanillian _dist/bin/

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