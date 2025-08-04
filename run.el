;;; run.el --- Script loading and interacting with Hamacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(add-to-list 'load-path "packages")
(require 'hamacs)
(let ((hint-params
       (append
        (list "-no-user-package-db"
              "-package-env" "-"
              "-package-db" (getenv "NIX_GHC_LIBDIR"))
        (mapcan (lambda (x)
                  (let ((prefixed-x (file-name-concat (getenv "PWD") x)))
                    (if (file-exists-p prefixed-x)
                        (list "-package-db" prefixed-x)
                      nil)))
                (list "dist-newstyle/packagedb/ghc-9.12.2" "dist/package.conf.inplace")))))
  (hamacs-load-package hint-params "hapack"))

(cl-assert (hapack-sayHello) t)
(hapack-iAmEmacsCompatibleFun "wow")
(message "Result: %d" (hapack-fooBar 22))
(message "Result: %d" (hapack-third_function 11 1000))
(hapack-sayHelloInEmacs "This message is emitted via Emacs message function")
(cl-assert (eq t (hapack-sayHelloFromSaveExcursion)) t)
(cl-assert (eq 3333 (hapack-fooBar0FromSaveExcursion)) t)
(hapack-runHamacsApiTests)
(cl-assert (null (commandp 'hapack-fooBar)))
; (cl-assert (commandp 'hapack-sayHello))
;;; run.el ends here
