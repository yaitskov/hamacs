;;; run.el --- Script loading and interacting with Hamacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(add-to-list 'load-path "packages")
(message "Before mymodule is required")
(require 'hamacs)
(message "After mymodule is required")
(let ((hint-params
       (list "-no-user-package-db"
             "-package-env" "-"
             "-package-db" (getenv "NIX_GHC_LIBDIR")
             "-package-db" (file-name-concat (getenv "PWD") "dist/package.conf.inplace")
       )))
  (hamacs-load-package hint-params "hapack"))

(cl-assert (hapack-sayHello) t)
(hapack-iAmEmacsCompatibleFun "wow")
(message "Result: %d" (hapack-fooBar 22))
(message "Result: %d" (hapack-third_function 11 1000))
(hapack-sayHelloInEmacs "This message is emitted via Emacs message function")
(cl-assert (eq t (hapack-sayHelloFromSaveExcursion)) t)
(cl-assert (eq 3333 (hapack-fooBar0FromSaveExcursion)) t)
;;; run.el ends here
