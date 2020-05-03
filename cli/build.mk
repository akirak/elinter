.PHONY: build
build:
	spago build
	spago bundle-app --to dist.js
