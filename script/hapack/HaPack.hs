module HaPack where


import Emacs.Core (mkFunctionFromCallable, message)
import Emacs.Type
import Relude

fooBar :: Int -> Int
fooBar n = n + 33

fooBar0 :: Int
fooBar0 = 3333

sayHello :: EmacsM ()
sayHello = putStrLn "Hello from HINT"

third_function :: Int -> Int -> Int
third_function a b = a + b

iAmEmacsCompatibleFun :: Text -> EmacsM ()
iAmEmacsCompatibleFun txt = message txt

($$$$) :: Int -> Int -> Int
($$$$) = (+)
