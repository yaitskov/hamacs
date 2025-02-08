test-sync: build
	cd integration && make
build:
	cabal build
	sync
