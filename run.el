;;; run.el --- Testing Hamacs uninstallated Hamacs integration with Emacs  -*- lexical-binding: t; -*-
;;; Commentary: Inside nix-shell or in checkPhase.
;;; Code:

(require 'hamacs)

(let ((nix-ghc-libdir (getenv "NIX_GHC_LIBDIR")))
  (cl-assert (not (null nix-ghc-libdir)) "test relies on nix-shell env")
  (add-to-list 'load-path "packages")
  (hamacs-load-package-with-hint-args
   (append
    (list "-no-user-package-db"
          "-package-env" "-"
          "-package-db" nix-ghc-libdir)
    (mapcan (lambda (x)
              (let ((prefixed-x (file-name-concat (getenv "PWD") x)))
                (if (file-exists-p prefixed-x)
                    (list "-package-db" prefixed-x)
                  nil)))
            (list "dist-newstyle/packagedb/ghc-9.12.2" ; nix-shell
                  "dist/package.conf.inplace")))       ; nix-build checkPhase
   "hapack"))

(add-to-list 'load-path "packages/hapack")
(require 'hapack-tests)
(run-hapack-tests)

;;; run.el ends here
