test-sync:  build
	emacs --batch --no-init-file \
		-eval "(module-load \"$(PWD)/$(shell find -name *eli*so)\")" \
		-eval "(hint-how-are-you)" \
		-eval "(eval-in-calling-thread \"Prelude.putStrLn (show ['M', 'e', 's', 's', 'a', 'g', ' ', 'f', 'r', 'o', 'H', 'i'])\")"
# -eval "(message \"and %s\" (mysquare 3))" \
# -eval "(message \"1+2 = %s\" (myplus 1 2))" \
# -eval "(hint-how-are-you)"
  #		-eval "(sleep-for 3)"

# -eval "(eval-haskell \"Prelude.putStrLn (show True)\")"
build:
	cabal build
	sync
