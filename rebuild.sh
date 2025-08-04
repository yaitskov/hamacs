#!/bin/bash

set -x
set -e

cabal build && \
    ln -s -f $PWD/dist-newstyle/build/x86_64-linux/ghc-9.12.2/hamacs-0.0.1/build/*so hamacs.so
