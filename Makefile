all: npm-deps purs-deps bundle

nix:
	nix-shell --run 'make all'

bundle:
	purp build
	purp bundle

npm-deps:
	npm install

purs-deps:
	nix-shell install-deps.nix --run 'echo installation complete.'

psc-package2nix:
	psc-package2nix
