;;; run.el --- Testing Hamacs integration with Emacs.     -*- lexical-binding: t; -*-
;;; Commentary: Hamacs operates outside of nix-shell.
;;; Code:
(add-to-list 'load-path "packages")
(require 'hamacs)

(cl-assert (null (getenv "NIX_GHC_LIBDIR"))
           "testing should be conducted outside of nix-shell")

(hamacs-load-package "hapack") ; sunny day case - NIX_GHC_LIBDIR is fabricated inside hamacs

(require 'hapack-tests)
(run-hapack-tests)

;;; run.el ends here
