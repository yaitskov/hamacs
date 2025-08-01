;;; run.el --- Script loading and interacting with Hamacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(add-to-list 'load-path "packages")
(message "Before mymodule is required")
(require 'hamacs)
(message "After mymodule is required")
(hamacs-load-package "hapack")
(hapack-sayHello)
(hapack-iAmEmacsCompatibleFun "wow")
(message "Result: %d" (hapack-fooBar 22))
(message "Result: %d" (hapack-third_function 11 1000))
(hapack-sayHelloInEmacs "This message is emitted via Emacs message function")
(hapack-sayHelloFromSaveExcursion)
;;; run.el ends here
