module HaPack where


import Relude
import Emacs.Type

fooBar :: Int
fooBar = 33

sayHello :: EmacsM ()
sayHello = putStrLn "Hello from HINT"

third_function :: Int -> Int -> IO ()
third_function a b = print $ a + b


($$$$) :: Int -> Int -> Int
($$$$) = (+)
