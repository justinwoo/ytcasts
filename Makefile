nix:
	nix-shell --run 'make all'

all: npm-deps purs-deps bundle

bundle:
	purp build
	purp bundle

npm-deps:
	npm install

purs-deps:
	psc-package2nix
	nix-shell install-deps.nix --run 'echo installation complete.'
