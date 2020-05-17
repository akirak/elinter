.PHONY: build
build: update-version
	spago build
	spago bundle-app --to dist.js

.PHONY: install
install: build
	nix-env -if .
	melpa-check --version

.PHONY: update-version
update-version:
	emacs --batch -Q -l cli-version.el -f cli-version-update

.PHONY: check-version
check-version:
	emacs --batch -Q -l cli-version.el -f cli-version-check
