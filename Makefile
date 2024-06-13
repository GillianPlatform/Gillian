build:
	dune build @all

init-dev:
	opam switch create . --packages=ocaml-variants.5.2.0+options,ocaml-lsp-server,ocamlformat,feather,fileutils,odoc.0.26.2 -y --deps-only --locked
	opam exec -- ./githooks/install.ml
	
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