gh-dist:
	NIX_BUILD_SHELL=bash nix-shell --command "spago2nix build"
	nix build
	cp -v result/index.js dist.js

.PHONY: gh-dist
