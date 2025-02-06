test:  build

	emacs --batch --no-init-file \
		-eval "(module-load \"$(PWD)/$(shell find -name *eli*so)\")" \
		-eval "(message \"and %s\" (mysquare 3))"
build:
	cabal build
	sync
