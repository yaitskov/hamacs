test-sync:
	cd integration && make

build:
	cabal build
	cabal install  --force-reinstalls --lib
	sync
