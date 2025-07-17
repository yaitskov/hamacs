#!/bin/bash

set -x
set -e

cabal build && \
    cp $PWD/dist-newstyle/build/x86_64-linux/ghc-9.12.2/hamacs-0.0.1/build/*so mymodule.so && \
    emacs -Q -L $PWD --batch \
          --execute "(message \"Before mymodule is required\")" \
          --execute "(require 'mymodule)" \
          --execute "(message \"After mymodule is required\")" \
          --execute "(hint-how-are-you)" \
          --execute "(message (number-to-string (myplus 10 1000)))" \
          --execute "(hint-how-are-you)" \
          --execute "(message (number-to-string (myplus 10 7)))"
