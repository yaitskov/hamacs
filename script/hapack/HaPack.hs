module HaPack where


import Emacs.Core (mkFunctionFromCallable, message)
import Emacs.Type
import Relude

fooBar :: Int
fooBar = 33

sayHello :: EmacsM ()
sayHello = putStrLn "Hello from HINT"

third_function :: Int -> Int -> IO ()
third_function a b = print $ a + b

iAmEmacsCompatibleFun :: Text -> EmacsM ()
iAmEmacsCompatibleFun txt = message txt

($$$$) :: Int -> Int -> Int
($$$$) = (+)
