;;; package --- Script loading and interacting with Hamacs
;;; Commentary:
;;; Code:
(message "Before mymodule is required")
(require 'hamacs)
(message "After mymodule is required")
(hamacs-load-package "hapack")
(hamacs-ping-package "hapack")
(hamacs-ping-package "hapack")
(hapack-sayHello)
(hapack-iAmEmacsCompatibleFun "wow")
(message "Result: %d" (hapack-fooBar 22))
(message "Result: %d" (hapack-third_function 11 1000))
(hapack-sayHelloInEmacs "This message is emitted via Emacs message function")
;;; run.el ends here
