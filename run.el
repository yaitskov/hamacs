;;; package --- Script loading and interacting with Hamacs
;;; Commentary:
;;; Code:
(message "Before mymodule is required")
(require 'mymodule)
(message "After mymodule is required")
(eval-in-calling-thread "sayHello")
(eval-in-calling-thread "sayHello")
(hint-how-are-you)
(message (number-to-string (myplus 10 1000)))
(hint-how-are-you)
(message (number-to-string (myplus 10 7)))
(eval-haskell "putStrLn \"Hello Наконец Работает !!!! World\"")
(eval-haskell "putStrLn \"Second message in the Queue\"")
(eval-haskell "putStrLn \"Third message in the Queue\"")
(hint-how-are-you)
(message (number-to-string (myplus 11111 22222)))
(load-hamacs-package "HelloBoom")
;;; run.el ends here
