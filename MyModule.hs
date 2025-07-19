module MyModule where

import Relude
import Emacs.Type

fooBar :: Int
fooBar = 33

sayHello :: EmacsM ()
sayHello = putStrLn "Hello from HINT"
