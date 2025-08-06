(require 'hamacs)

(defun run-hapack-tests ()
  "integration tests for Hamacs."
  (cl-assert (hapack-sayHello) t)
  (hapack-iAmEmacsCompatibleFun "wow")
  (message "Result: %d" (hapack-fooBar 22))
  (message "Result: %d" (hapack-third_function 11 1000))
  (hapack-sayHelloInEmacs "This message is emitted via Emacs message function")
  (cl-assert (eq t (hapack-sayHelloFromSaveExcursion)) t)
  (cl-assert (eq 3333 (hapack-fooBar0FromSaveExcursion)) t)
  (hapack-runHamacsApiTests)
  (cl-assert (null (commandp 'hapack-fooBar)))
  (cl-assert (commandp 'hapack-sayHello)))

(provide 'hapack-tests)
