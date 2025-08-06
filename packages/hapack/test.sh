#!/bin/bash

set -e

nix-shell \
    --extra-experimental-features nix-command \
    --extra-experimental-features pipe-operators \
    --run mk_link_to_hamacs \
    --argstr nix-ghc-libdir-out-file /stub-nix-ghc-libdir-out-file \
    nix-ghc-libdir-setup.nix

./emacs -Q -L . --batch -l run.el
