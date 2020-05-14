.PHONY: build
build:
	spago build
	spago bundle-app --to dist.js

.PHONY: install
install: build
	nix-env -if .
	melpa-check --version
